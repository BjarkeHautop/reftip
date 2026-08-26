# Doxygen-style hover tooltip, rendered as a single `position: fixed` div
# shared by the whole page and positioned by a few lines of vanilla JS.
#
# A pure-CSS `:hover` sibling (no JS) was the first attempt, but Quarto's own
# stylesheet sets `overflow: auto` on the code-block wrapper so long lines
# scroll horizontally -- and that clips any absolutely-positioned descendant,
# including a tooltip box nested inside the same `<pre>`. `position: fixed`
# is positioned relative to the viewport, exactly like the coordinates
# `getBoundingClientRect()` returns, so it escapes that clipping regardless
# of which of altdoc's four backends rendered the page. This mirrors why
# DocumenterCodeBlocks.jl's ref-popup.js does the same thing rather than a
# CSS-only tooltip (see its assets/ref-popup.{css,js}).
#
# Both blocks are marked with an HTML comment so a rerun of `add_tooltips()`
# can detect an already-processed file and skip it (see `add_tooltips()`),
# rather than injecting a second payload/script.
.REFTIP_MARKER <- "<!-- reftip -->"

.reftip_css_block <- function() {
    paste0(
        .REFTIP_MARKER, "\n",
        "<style>\n",
        ".reftip-float{display:none;position:fixed;z-index:10000;",
        "min-width:220px;max-width:420px;",
        "background:#1e1e1e;color:#eee;border:1px solid #444;",
        "border-radius:6px;padding:.5em .75em;font-size:.85em;",
        "line-height:1.4;box-shadow:0 4px 14px rgba(0,0,0,.35);",
        "white-space:normal;text-align:left;font-weight:normal;",
        "font-style:normal;}\n",
        ".reftip-float .reftip-sig{display:block;white-space:pre-wrap;",
        "font-family:ui-monospace,SFMono-Regular,Consolas,monospace;",
        "font-weight:600;color:#9cdcfe;margin-bottom:.35em;}\n",
        ".reftip-float .reftip-brief{margin:0;color:#ccc;}\n",
        "a.reftip-ref{cursor:help;}\n",
        "</style>\n"
    )
}

# `tips` is a named list, id -> list(usage, brief); rendered once per page as
# a hidden payload (`ref-popup.js`'s `.ref-tips` idea) that the script below
# reads by `data-for`. `data-reftip` on each link is the same id, assigned by
# `.inject_tooltips_html()` in first-appearance order -- ids rather than raw
# names sidestep HTML-attribute escaping entirely.
.reftip_payload_html <- function(tips) {
    entries <- vapply(names(tips), function(id) {
        t <- tips[[id]]
        sig <- if (!is.null(t$usage)) t$usage else id
        brief_html <- if (!is.null(t$brief)) {
            paste0("<span class=\"reftip-brief\">", .html_escape(t$brief), "</span>")
        } else {
            ""
        }
        sprintf(
            '<div class="reftip-tip" data-for="%s"><code class="reftip-sig">%s</code>%s</div>',
            id, .html_escape(sig), brief_html
        )
    }, character(1))

    paste0(
        '<div class="reftip-payload" hidden>', paste(entries, collapse = ""), "</div>\n"
    )
}

.reftip_script_block <- function() {
    paste0(
        "<script>\n",
        "(function(){\n",
        "  var payload = document.currentScript.previousElementSibling;\n",
        "  var float = document.createElement('div');\n",
        "  float.className = 'reftip-float';\n",
        "  float.setAttribute('role', 'tooltip');\n",
        "  document.body.appendChild(float);\n",
        "  var hideTimer;\n",
        "  function place(el){\n",
        "    var r = el.getBoundingClientRect();\n",
        "    var top = r.bottom + 6;\n",
        "    var left = r.left;\n",
        "    var maxLeft = window.innerWidth - float.offsetWidth - 8;\n",
        "    if (left > maxLeft) left = Math.max(8, maxLeft);\n",
        "    var maxTop = window.innerHeight - float.offsetHeight - 8;\n",
        "    if (top > maxTop) top = Math.max(8, r.top - float.offsetHeight - 6);\n",
        "    float.style.top = top + 'px';\n",
        "    float.style.left = left + 'px';\n",
        "  }\n",
        "  function show(el){\n",
        "    var key = el.getAttribute('data-reftip');\n",
        "    var tip = payload.querySelector('[data-for=\"' + key + '\"]');\n",
        "    if (!tip) return;\n",
        "    float.innerHTML = tip.innerHTML;\n",
        "    float.style.display = 'block';\n",
        "    place(el);\n",
        "  }\n",
        "  function hide(){ float.style.display = 'none'; }\n",
        "  function cancelHide(){ clearTimeout(hideTimer); }\n",
        "  function scheduleHide(){ hideTimer = setTimeout(hide, 150); }\n",
        "  var links = document.querySelectorAll('a.reftip-ref');\n",
        "  for (var i = 0; i < links.length; i++) {\n",
        "    (function(el){\n",
        "      el.addEventListener('mouseenter', function(){ cancelHide(); show(el); });\n",
        "      el.addEventListener('mouseleave', scheduleHide);\n",
        "      el.addEventListener('focus', function(){ show(el); });\n",
        "      el.addEventListener('blur', hide);\n",
        "    })(links[i]);\n",
        "  }\n",
        "  float.addEventListener('mouseenter', cancelHide);\n",
        "  float.addEventListener('mouseleave', scheduleHide);\n",
        "})();\n",
        "</script>\n"
    )
}
