---
layout: post
author: Yusef Aslam
tags: emacs-lisp, html
comments: true
---

Recently I searched for a way to use Emacs Lisp to write HTML, I found a library called "charge" from this guy which lets you do exactly that: [https://hungyi.net/posts/dead-simple-emacs-lisp-templating/](https://hungyi.net/posts/dead-simple-emacs-lisp-templating/)

With these functions you can write HTML using Elisp like this for example: 
```elisp
(html 
    (body 
        (a :href "#"))) 
```
And it would output HTML like: 

```html
<html>
    <body>
        <a href="#"></a>
    </body>
</html>
```

As you can see in the above code block, closing tags are generated automatically.

This is quite powerful, because you don't have to worry about tags etc.. it's all parenthesis, and lets you write HTML quicker. 

I wrote a function that uses this library and after evaluating the function, opens up a buffer in a new frame with the generated content. 

```elisp
(require 'charge)
(require 'web-mode)
(defun ysz-generate-html (html)
  (let ((buffer "*generated-file*"))
    (if (not (bufferp (get-buffer buffer)))
        (generate-new-buffer buffer)
      (kill-buffer buffer)
      (generate-new-buffer buffer))

    (with-current-buffer buffer
      (insert
        (charge-html html))
      (if (not (functionp 'sgml-pretty-print))
          (sgml-mode)
       (mark-whole-buffer)
       (sgml-pretty-print (point-min) (point-max))
       (if (not (string-equal major-mode "#'web-mode"))
           (web-mode))
       (web-mode-buffer-indent)

       (display-buffer-pop-up-frame buffer '((nil)))

       (if (not (bufferp (get-buffer "generated.html")))
           (write-file "~/generated.html")
         (kill-buffer (get-buffer "generated.html"))
         (write-file "~/generated.html"))))))

```

You would call it like this:

```elisp
(ysz-generate-html
 `(html
   (head
     (link :rel "stylesheet" :href "#"))
   (body (a :href "#")
      (div :class "site-container"
         (div :id "page"
              (div :id "list"
                (ul
                  (li "Stuff"))))))))
```

Note the backtick before the `(html` bit, this is needed since these aren't functions, just symbols.

Which would for example return this in a new popup frame:

```html
<html>
    <head>
        <link rel="stylesheet" href="#"/>
    </head>
    <body>
        <a href="#">
        </a>
        <div class="site-container">
            <div id="page">
                <div id="list">
                    <ul>
                        <li>Stuff
                        </li>
                    </ul>
                </div>
            </div>
        </div>
    </body>
</html>
```

And you could even so some quirky stuff like making a vertical list:

```elisp
(ysz-generate-html
 `(html
   (head
     (link :rel "stylesheet" :href "#"))
   (body (a :href "#")
      (div :class "site-container"
         (div :id "page"
              (div :id "list"
                (ul
                 ,(mapcar (lambda (x) `(li, (char-to-string x)))
                          (current-time-string)))))))))
```

Note that you can use normal Emacs functions by adding a comma before the function.

This would return HTML like:
```html
<html>
    <head>
        <link rel="stylesheet" href="#"/>
    </head>
    <body>
        <a href="#">
        </a>
        <div class="site-container">
            <div id="page">
                <div id="list">
                    <ul>
                        <li>T
                        </li>
                        <li>h
                        </li>
                        <li>u
                        </li>
                        <li>
                        </li>
                        <li>M
                        </li>
                        <li>a
                        </li>
                        <li>r
                        </li>
                        <li>
                        </li>
                        <li>
                        </li>
                        <li>2
                        </li>
                        <li>
                        </li>
                        <li>1
                        </li>
                        <li>2
                        </li>
                        <li>:
                        </li>
                        <li>4
                        </li>
                        <li>0
                        </li>
                        <li>:
                        </li>
                        <li>2
                        </li>
                        <li>3
                        </li>
                        <li>
                        </li>
                        <li>2
                        </li>
                        <li>0
                        </li>
                        <li>2
                        </li>
                        <li>3
                        </li>
                    </ul>
                </div>
            </div>
        </div>
    </body>
</html>
```

Which looks like:
<html>
    <head>
        <link rel="stylesheet" href="#"/>
    </head>
    <body>
        <a href="#">
        </a>
        <div class="site-container">
            <div id="page">
                <div id="list">
                    <ul>
                        <li>T
                        </li>
                        <li>h
                        </li>
                        <li>u
                        </li>
                        <li>
                        </li>
                        <li>M
                        </li>
                        <li>a
                        </li>
                        <li>r
                        </li>
                        <li>
                        </li>
                        <li>
                        </li>
                        <li>2
                        </li>
                        <li>
                        </li>
                        <li>1
                        </li>
                        <li>2
                        </li>
                        <li>:
                        </li>
                        <li>4
                        </li>
                        <li>0
                        </li>
                        <li>:
                        </li>
                        <li>2
                        </li>
                        <li>3
                        </li>
                        <li>
                        </li>
                        <li>2
                        </li>
                        <li>0
                        </li>
                        <li>2
                        </li>
                        <li>3
                        </li>
                    </ul>
                </div>
            </div>
        </div>
    </body>
</html>

This lets you quickly template HTML and is very useful. 

If you have any questions or improvements to this code, please post them in the comments section, I hope this can help you, thanks for reading!
