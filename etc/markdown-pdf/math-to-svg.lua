local cache = {}

local latex_template = [[
\documentclass[border=0.5pt]{standalone}
\usepackage{amsmath,amssymb}
\usepackage{fontspec}
\usepackage{xeCJK}
\setCJKmainfont{Songti SC}
\begin{document}
%s
\end{document}
]]

local function read_file(path)
  local file = assert(io.open(path, "rb"))
  local contents = file:read("*all")
  file:close()
  return contents
end

local function write_file(path, contents)
  local file = assert(io.open(path, "wb"))
  file:write(contents)
  file:close()
end

local function render_math(element, temporary_directory)
  local text = element.text:match("^%s*(.-)%s*$")
  local key = element.mathtype .. "\0" .. text
  local cached = cache[key]
  local class = element.mathtype == "DisplayMath"
      and "math-display" or "math-inline"

  if not cached then
    local hash = pandoc.sha1(key)
    local basename = "math-" .. hash
    local tex_path = pandoc.path.join({temporary_directory, basename .. ".tex"})
    local pdf_path = pandoc.path.join({temporary_directory, basename .. ".pdf"})
    local svg_path = pandoc.path.join({temporary_directory, basename .. ".svg"})
    local math = element.mathtype == "DisplayMath"
        and ("\\(\\displaystyle\n" .. text .. "\n\\)")
        or ("\\(" .. text .. "\\)")
    local latex_source = string.format(latex_template, math)

    write_file(tex_path, latex_source)

    local ok, message = pcall(
      pandoc.pipe,
      "xelatex",
      {
        "-interaction=nonstopmode",
        "-halt-on-error",
        "-no-shell-escape",
        "-output-directory=" .. temporary_directory,
        tex_path
      },
      ""
    )
    if not ok then
      error("Unable to render Markdown math `" .. text .. "`: "
            .. tostring(message) .. "\nGenerated TeX:\n" .. latex_source)
    end

    pandoc.pipe("pdf2svg", {pdf_path, svg_path}, "")
    local svg = read_file(svg_path)
    local filename = basename .. ".svg"
    pandoc.mediabag.insert(filename, "image/svg+xml", svg)
    cached = filename
    cache[key] = filename
  end

  return pandoc.Image(
    {pandoc.Str(text)},
    cached,
    "",
    pandoc.Attr("", {class})
  )
end

function Pandoc(document)
  return pandoc.system.with_temporary_directory(
    "thy-markdown-math",
    function (temporary_directory)
      return document:walk({
        Math = function (element)
          return render_math(element, temporary_directory)
        end
      })
    end
  )
end
