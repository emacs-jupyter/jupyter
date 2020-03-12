(source gnu)
(source melpa)
(source org)

(package-file "jupyter.el")

(files "*.el" "jupyter.png" "widget.html"
       ("js"
        ("widget" "js/widget/*"
         (:exclude "js/widget/node_modules" "js/widget/built"
                   "js/widget/*lock*"))
        ("web-renderer" "js/web-renderer/*")))

(development
 (depends-on "ert-runner")
 (depends-on "org-plus-contrib")
 (depends-on "julia-mode"))
