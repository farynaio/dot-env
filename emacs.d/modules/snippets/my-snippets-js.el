(eval-after-load 'tempo
  '(progn
     (defvar my/tempo-js-tags nil
       "Tags for JS mode.")

     (tempo-define-template "js-console-log"
       '("console.log(" ~ ")")
       "console"
       "Insert console.log"
       'my/tempo-js-tags)

     (tempo-define-template "js-stateless-component"
       '("const " (p "name: " name) " = () => {" n>
          "return (" n>
          ~ n>
          ")" > n>
          "}" > n n
          "export default " (s name)
          )
       "sless"
       "Insert stateless component"
       'my/tempo-js-tags)

     (tempo-define-template "js-arrow-function"
       '("(" ~ ") => ")
       "=>"
       "Insert arrow function"
       'my/tempo-js-tags)

     (tempo-define-template "js-exportf"
       '("export default function " (p "name: " name) "() {" n>
          "return (" n>
          ~ n>
          ")" > n>
          "}" > n
          )
       "exportf"
       "Insert 'export default function'"
       'my/tempo-js-tags)

     (tempo-define-template "js-exportc"
       '("export const " ~ " = " >)
       "exportc"
       "Insert 'export const'"
       'my/tempo-js-tags)

     (tempo-define-template "js-todo-tag"
       '("// TODO " ~)
       "todo"
       "Insert TODO block"
       'my/tempo-js-tags)

     (tempo-define-template "js-cn-tag"
       '("className=\"" ~ "\"")
       "cn"
       "Insert className=\"\""
       'my/tempo-js-tags)
     ))

(provide 'my-snippets-js)
;;; my-snippets-js.el ends here