-- Rewrite Quarto @sec- cross-references to: Title (Section N)
--
-- Runs after Quarto's filters. For PDF, Quarto emits
--   Str("Section") + nbsp + RawInline("\\ref{sec-...}")
-- For HTML, Quarto emits a Link to #sec-... with text "Section N".
-- This filter prefixes the section title and wraps the numbered
-- reference in parentheses, e.g. "Missing data (Section 5.3)".

local sections = {}

local function escape_lua_pattern(s)
  return (s:gsub("(%W)", "%%%1"))
end

local function header_title(el)
  local title = pandoc.utils.stringify(el.content)
  local num = el.attributes.number
  if num and num ~= "" then
    title = title:gsub("^" .. escape_lua_pattern(num) .. "%s+", "")
  end
  return title
end

local function store_header(el)
  if el.identifier ~= "" and el.identifier:match("^sec%-") then
    sections[el.identifier] = {
      title = header_title(el),
      number = el.attributes.number
    }
  end
  return nil
end

local function is_nbsp(el)
  return el.t == "Str" and (el.text == "\u{00A0}" or el.text == " ")
end

local function is_section_word(el)
  return el.t == "Str" and (el.text == "Section" or el.text == "section")
end

local function ref_id_from_raw(el)
  if el.t ~= "RawInline" or (el.format ~= "latex" and el.format ~= "tex") then
    return nil
  end
  return el.text:match("\\ref%{([%w%-]+)%}")
end

local function rewrite_inlines(inlines)
  local result = pandoc.List()
  local i = 1
  while i <= #inlines do
    local a = inlines[i]
    local b = inlines[i + 1]
    local c = inlines[i + 2]

    -- PDF / LaTeX: Section + nbsp + \ref{sec-...}
    if a and b and c and is_section_word(a) and is_nbsp(b) then
      local id = ref_id_from_raw(c)
      local info = id and sections[id]
      if info then
        result:insert(pandoc.Str(info.title))
        result:insert(pandoc.Space())
        result:insert(pandoc.Str("("))
        result:insert(a)
        result:insert(b)
        result:insert(c)
        result:insert(pandoc.Str(")"))
        i = i + 3
      else
        result:insert(a)
        i = i + 1
      end

    -- HTML / docx-style: Link to #sec-...
    elseif a and a.t == "Link" and a.target:match("^#sec%-") then
      local id = a.target:sub(2)
      local info = sections[id]
      if info then
        local content = pandoc.List()
        content:insert(pandoc.Str(info.title))
        content:insert(pandoc.Space())
        content:insert(pandoc.Str("("))
        for _, inline in ipairs(a.content) do
          content:insert(inline)
        end
        content:insert(pandoc.Str(")"))
        result:insert(pandoc.Link(content, a.target, a.title, a.attr))
      else
        result:insert(a)
      end
      i = i + 1

    else
      result:insert(a)
      i = i + 1
    end
  end
  return result
end

function Pandoc(doc)
  doc = doc:walk({ Header = store_header })
  return doc:walk({
    Inlines = function(inlines)
      return rewrite_inlines(inlines)
    end
  })
end
