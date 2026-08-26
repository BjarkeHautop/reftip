# One `position: fixed` tooltip div shared by the whole page, positioned by
# a few lines of vanilla JS. `position: fixed` (rather than a pure-CSS
# `:hover` sibling) avoids Quarto's code-block wrapper clipping it via
# `overflow: auto`.
#
# Marks output with an HTML comment so a rerun of `add_tooltips()` can skip
# an already-processed file.
.REFTIP_MARKER <- "<!-- reftip -->"

.reftip_css_block <- function() {
    paste0(
        .REFTIP_MARKER, "\n",
        "<style>\n",
        ".reftip-float{display:none;position:fixed;z-index:10000;",
        "min-width:220px;max-width:420px;",
        "background:var(--bs-body-bg,#1e1e1e);color:var(--bs-body-color,#eee);",
        "border:1px solid var(--bs-border-color,#444);",
        "border-radius:6px;padding:.5em .75em;font-size:.85em;",
        "line-height:1.4;box-shadow:0 4px 14px rgba(0,0,0,.35);",
        "white-space:normal;text-align:left;font-weight:normal;",
        "font-style:normal;}\n",
        ".reftip-float .reftip-sig{display:block;white-space:pre-wrap;",
        "font-family:ui-monospace,SFMono-Regular,Consolas,monospace;",
        "font-weight:600;color:var(--bs-link-color,#9cdcfe);margin-bottom:.35em;",
        # undo Bootstrap's code{} background so this doesn't look boxed in dark mode
        "background-color:transparent;padding:0;border-radius:0;}\n",
        ".reftip-float .reftip-brief{margin:0;color:var(--bs-secondary-color,#ccc);}\n",
        "a.reftip-ref{cursor:help;}\n",
        "</style>\n"
    )
}

# `tips`: id -> list(usage, brief, name), rendered as a hidden payload the
# script below reads via `data-for`/`data-reftip`.
.reftip_payload_html <- function(tips) {
    entries <- vapply(names(tips), function(id) {
        t <- tips[[id]]
        sig <- if (!is.null(t$usage)) t$usage else t$name
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
