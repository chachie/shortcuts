#  Browser shortcuts in Emacs

The `Shortcuts` module allows you to define commands in Emacs to quickly access web-pages and files.

## Use cases

  * Opening one or multiple distinct URLs with a single command
    * Opening a single URL is the equivalent of a browser bookmark
  * Opening one or multiple web-pages from a single [parameterized URL](https://developer.mozilla.org/en-US/docs/Learn/Common_questions/Web_mechanics/What_is_a_URL#parameters)
    * The URL is completed by prompting the user for the missing parameters
  * Copying the URL(s) produced by the above commands (instead of opening)
    * Used for example when sharing the URL(s) in an email or document

The use cases above are also applicable when opening files instead of URLs.

## Advantages over browser bookmarks

  * Allows for browser agnostic bookmarks
  * Version controlled:
    * Shortcuts definitions are stored in the Emacs customization file, which can be version controlled.
  * Accessible from different systems (when the Emacs configuration is the same across these systems).
  * Allows browsing multiple URLs with a single command

## Example

Let's say you would like to navigate to the Wikipedia article about Emacs.

* Place the `shortcuts.el` in your load path.
* **M-x customize-option**
* Type `shortcuts-list`.
* Under `Shortcuts List`, hit `INS` to add a new shortcut.
* Under `Command Name`, type `wikipedia`.
* Select `URL Shortcuts`, then `browse-url` as the function to execute.
* Under `List of URLs`, hit `INS`, then type (including the key characters[^1]):
  * `https://en.wikipedia.org/wiki/🔑Article🔑`
* Finally, press **C-x C-s** to save.

Now, **M-x wikipedia** will prompt you for an article and will open it in the browser.


<details>
 <summary> <i>How this works</i> </summary>

When the customize option is saved, the `Shortcuts` code sets the command name `wikipedia` as an _interactive command_ that can be called by the user with `M-x`.

Specifically, it defines an interactive command that:

* optionally accepts user input, if the URL contains parameter prompts to form a URL
* calls the chosen function with this URL.

</details>

[^1]: Entering these special characters can be done in Emacs with the command **C-x 8 C-m**, followed by "KEY" or "ROBOT FACE".


# Shortcut Options

## Command Name

While the command name can be any string (see [Symbol Type](https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Type.html)), it is normally easier to choose a consistent pattern. For example, lowercase and dash separated words follows the style of many existing commands in Emacs.

## Type of Shortcut

Shortcuts are categorized into two types: URL shortcuts and File shortcuts. These are used simply to group the available functions (actions) to take on a shortcut. For example, a find-file function is respondible for opening a file. Hence, this function is group with the "File shortcuts" list.

## Functions

Functions determine the behavior to apply on a path or URL. For example, the function to open a URL in an external browser is `browse-url`. Other functions can be used here depending on what packages are installed in Emacs.

# Specifying parameters in URLs and file paths

A URL or a file path can include any number of parameters in any position.

Parameters define two features of the prompt that the user sees when the command is called:
  * the prompt text the user sees
  * the acceptable input the user can enter

A parameter must be surrounded with one of two unique characters not generally present in a URL or file path. These characters define the acceptable input the user can enter. The two valid types are string and lisp expressions. These characters can be changed by updating the corresponding custom variables as shown in the table below.

| Valid User Input | Standard Value          | Custom Variable                   |
|------------------|-------------------------|-----------------------------------|
| string           | 🔑 (KEY 0x1F511)        | link-string-replacement-separator |
| lisp expression | 🤖 (ROBOT FACE 0x1F916) | link-lisp-replacement-separator   |

Between these characters, any text can be entered. This text is used when prompting the user to enter the replacement text at the position where this parameter figures in the URL or file path.

## Lisp expressions as parameters

Lisp expressions are evaluated to produce a range of values, each mapping to a unique URL.

In the Wikipedia example from above, the URL could use a Lisp expression for the parameter: `https://en.wikipedia.org/wiki/🤖Article🤖`.

Now, **M-x wikipedia** will prompt you for a Lisp expression. Providing for example `(list "earth" "wind" "fire")` will result in three browser tabs opened for each of "earth", "wind" and "fire".

Note that when a URL includes multiple lisp parameters, there will be as many URLs opened as there are combinations of values produced by these parameters.

For example, specifying $M$ pull requests for $N$ repositories on github will result in opening $M \times N$ tabs.
  * `https://github.com/🔑Org🔑/🤖Repository🤖/pull/🤖Pull Request Number🤖`
