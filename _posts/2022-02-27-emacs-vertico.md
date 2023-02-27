---
author: Yusef Aslam
layout: post
tags: emacs
title: How to install Vertico inside Emacs
comments: true
toc: true
---


A Beginner's Guide to Installing Vertico in Emacs with `use-package`

![Vertico-Picture](https://github.com/minad/vertico/blob/screenshots/vertico-mx.png?raw=true)

Emacs is a powerful text editor that has been around for decades, and it has a vast ecosystem of packages that extend its capabilities. One such package is Vertico, which provides a flexible and efficient interface for searching and navigating through files, buffers, and other Emacs resources. In this tutorial, I'll show you how to install Vertico as a package in Emacs using the `use-package` macro.

## Prerequisites
Before we get started, you'll need to have the `use-package` macro installed. If you don't already have it, you can install it by running the following command in Emacs:

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
```

This code will check if the `use-package` package is installed, and if it's not, it will install it.

## Installing Vertico with `use-package`
Now that we have `use-package` installed, we can use it to install Vertico. Add the following code to your Emacs configuration file (usually `~/.emacs.d/init.el`):

```emacs-lisp
(use-package vertico
  :ensure t
  :init
  (vertico-mode))
```

This code tells `use-package` to install the Vertico package, ensure that it's installed, and enable Vertico mode when Emacs starts up.

## Testing Vertico
Save the changes to your Emacs configuration file, then restart Emacs or reload your configuration by running `M-x load-file RET ~/.emacs.d/init.el RET`.

Once Emacs has restarted, Vertico should be installed and enabled. You can test it out by opening any file and pressing C-x b to switch to a different buffer. You should see a prompt with a list of buffers, and you can start typing to filter the list.

## Conclusion
With Vertico installed, you can use its powerful search and navigation features to quickly find and open files, buffers, and other resources in Emacs. By using the `use-package` macro, you can easily install and manage packages like Vertico in your Emacs configuration.

