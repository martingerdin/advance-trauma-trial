#!/usr/bin/env python3
"""Transcribe a meeting audio/video file with OpenAI's diarized transcription.

Uses the ``gpt-4o-transcribe-diarize`` model and ``diarized_json`` output so the
result is a speaker-labelled transcript (speakers labelled A, B, ...).

Usage:
    python3 transcribe_meeting.py INPUT.m4a [--out DIR]

The OpenAI API key is read from $OPENAI_API_KEY if set, otherwise from
~/.openai_api_key. It is never printed or hardcoded.
"""
import argparse
import json
import mimetypes
import os
import pathlib
import subprocess
import sys
import tempfile
import uuid
import urllib.error
import urllib.request

API_URL = "https://api.openai.com/v1/audio/transcriptions"
MODEL = "gpt-4o-transcribe-diarize"  # diarized transcription model


def load_key():
    key = os.environ.get("OPENAI_API_KEY")
    if key:
        return key.strip()
    p = pathlib.Path.home() / ".openai_api_key"
    if p.exists():
        for line in p.read_text(encoding="utf-8").splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            if "=" in line:  # tolerate "OPENAI_API_KEY=sk-..."
                line = line.split("=", 1)[1]
            line = line.strip().strip('"').strip("'")
            if line:
                return line
    sys.exit(
        "No OpenAI API key found. Set OPENAI_API_KEY or create ~/.openai_api_key "
        "containing the key (chmod 600)."
    )


def build_multipart(payload, files):
    """Return (body_bytes, content_type) for a multipart/form-data request."""
    boundary = "----transcribe" + uuid.uuid4().hex
    buf = bytearray()
    for name, value in payload.items():
        buf += f"--{boundary}\r\n".encode()
        buf += f'Content-Disposition: form-data; name="{name}"\r\n\r\n'.encode()
        buf += str(value).encode() + b"\r\n"
    for name, filepath in files.items():
        data = pathlib.Path(filepath).read_bytes()
        fname = pathlib.Path(filepath).name
        ctype = mimetypes.guess_type(filepath)[0] or "application/octet-stream"
        buf += f"--{boundary}\r\n".encode()
        buf += f'Content-Disposition: form-data; name="{name}"; filename="{fname}"\r\n'.encode()
        buf += f"Content-Type: {ctype}\r\n\r\n".encode()
        buf += data + b"\r\n"
    buf += f"--{boundary}--\r\n".encode()
    return bytes(buf), f"multipart/form-data; boundary={boundary}"


def transcribe(key, audio_path, timeout=1800):
    payload = {
        "model": MODEL,
        "response_format": "diarized_json",
        "chunking_strategy": "auto",
        "language": "en",
    }
    body, ctype = build_multipart(payload, {"file": audio_path})
    req = urllib.request.Request(
        API_URL,
        data=body,
        method="POST",
        headers={
            "Authorization": f"Bearer {key}",
            "Content-Type": ctype,
        },
    )
    print(f"Uploading {audio_path} ({len(body)/1e6:.1f} MB) ...", file=sys.stderr)
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return json.loads(resp.read().decode("utf-8"))
    except urllib.error.HTTPError as e:
        detail = e.read().decode("utf-8", errors="replace")
        sys.exit(f"API error {e.code}: {detail}")
    except urllib.error.URLError as e:
        sys.exit(f"Network error: {e.reason}")


def formatted_transcript(data):
    """Render diarized segments as readable transcript lines."""
    lines = []
    for seg in data.get("segments", []):
        start = seg.get("start", 0)
        end = seg.get("end", 0)
        mm, ss = divmod(int(start), 60)
        hh, mm = divmod(mm, 60)
        ts = f"{hh:02d}:{mm:02d}:{ss:02d}"
        speaker = seg.get("speaker", "?")
        text = seg.get("text", "").strip()
        lines.append(f"[{ts}] {speaker}: {text}")
    return "\n".join(lines)


def split_audio(input_path, chunk_minutes, workdir):
    """Split audio into chunk_minutes pieces with ffmpeg (16 kHz mono).

    Returns a list of (chunk_path, start_seconds).
    """
    import subprocess as sp

    duration = float(
        sp.run(
            [
                "ffprobe", "-v", "error", "-show_entries", "format=duration",
                "-of", "default=noprint_wrappers=1:nokey=1", str(input_path),
            ],
            capture_output=True,
            text=True,
            check=True,
        ).stdout.strip()
    )
    chunk_len = chunk_minutes * 60
    chunks = []
    start = 0
    idx = 1
    while start < duration:
        out = pathlib.Path(workdir) / f"_chunk_{idx:02d}.m4a"
        sp.run(
            [
                "ffmpeg", "-y", "-v", "error", "-ss", f"{start}", "-t", f"{chunk_len}",
                "-i", str(input_path), "-ar", "16000", "-ac", "1", "-c:a", "aac",
                str(out),
            ],
            check=True,
        )
        chunks.append((out, start))
        start += chunk_len
        idx += 1
    return chunks


def merge_chunks(chunk_results):
    """Merge per-chunk diarized results into one dataset.

    Speaker labels restart per chunk (A, B, ...), so each is disambiguated as
    "A#1", "B#1", ... and timestamps are shifted by the chunk start offset.
    Speaker identity across chunks is reconciled later (naming pass).
    """
    all_segments = []
    total_duration = 0.0
    for offset, data in chunk_results:
        for seg in data.get("segments", []):
            tag = f"{seg.get('speaker', '?')}#{offset // 60:02d}min"
            all_segments.append(
                {
                    "start": seg.get("start", 0) + offset,
                    "end": seg.get("end", 0) + offset,
                    "speaker": tag,
                    "text": seg.get("text", ""),
                }
            )
        total_duration = max(total_duration, offset + data.get("duration", 0))
    all_segments.sort(key=lambda s: s["start"])
    return {"duration": total_duration, "segments": all_segments}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", help="audio/video file to transcribe")
    parser.add_argument("--out", default=None, help="output directory (default: next to input)")
    parser.add_argument(
        "--chunk-minutes", type=int, default=12,
        help="split long files into chunks of this many minutes (upload size limit)",
    )
    args = parser.parse_args()

    audio = pathlib.Path(args.input)
    if not audio.exists():
        sys.exit(f"Input not found: {audio}")
    out_dir = pathlib.Path(args.out) if args.out else audio.parent / "transcripts"
    out_dir.mkdir(parents=True, exist_ok=True)

    key = load_key()
    stem = audio.stem

    with tempfile.TemporaryDirectory(prefix="transcribe_") as tmp:
        chunks = split_audio(str(audio), args.chunk_minutes, tmp)
        chunk_results = []
        for chunk_path, start in chunks:
            print(f"\n--- Chunk {chunk_path.name} (offset {start}s) ---", file=sys.stderr)
            data = transcribe(key, str(chunk_path))
            chunk_results.append((start, data))
        data = merge_chunks(chunk_results)

    raw_path = out_dir / f"{stem}.diarized.json"
    txt_path = out_dir / f"{stem}.txt"
    raw_path.write_text(json.dumps(data, indent=2), encoding="utf-8")
    txt_path.write_text(formatted_transcript(data), encoding="utf-8")

    duration = data.get("duration")
    n_segments = len(data.get("segments", []))
    print(f"Duration: {duration}s | segments: {n_segments}", file=sys.stderr)
    print(f"Saved:    {raw_path}", file=sys.stderr)
    print(f"Saved:    {txt_path}", file=sys.stderr)


if __name__ == "__main__":
    main()
