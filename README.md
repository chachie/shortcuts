#  Browser shortcuts in Emacs

The `Shortcuts` module allows you to define commands in Emacs to quickly open web-pages and files.

## Use cases

  * Opening one or multiple distinct URLs with a single command
    * _Opening a single URL is the equivalent of a browser bookmark_
  * Opening one or multiple web-pages from a single parameterized URL
    * _The URL is completed by prompting the user for the parameters_
  * Copying the URL(s) produced by the above commands (instead of opening)
    * _Used for example when sharing the URL(s) in an email or document_

The use cases above are also applicable when opening files instead of URLs.

## Advantages over browser bookmarks

  * Allows for browser agnostic bookmarks
  * Shortcuts definitions are stored in the Emacs customization file, which can be version controlled
  * Accessible from different systems (when the Emacs configuration is the same across these systems)
  * Allows browsing multiple URLs with a single command

## Example

Let's say you would like to open to the Wikipedia article about Emacs.

* Place the `shortcuts.el` in your load path.
* **M-x customize-option**
* Type `shortcuts-list`.
* Under `Shortcuts List`, hit `INS` to add a new shortcut.
* Under `Command Name`, type `wikipedia`.
* Select `URL Shortcuts`, then `browse-url` as the function to call.
* Under `List of URLs`, hit `INS`, then type (including the key character[^1]):
  * `https://en.wikipedia.org/wiki/🔑Article🔑`
* Finally, press **C-x C-s** to save.

Now, **M-x wikipedia** will prompt you for an article and will open it in the browser.


<details>
 <summary> <i>How this works</i> </summary>

When the customize option is saved, the `Shortcuts` module sets the command name `wikipedia` as an [_interactive command_](https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Commands.html) that can be called by the user with `M-x`.

Specifically, it defines an interactive command that:

* optionally accepts user input, if the URL contains parameter prompts to complete a URL
* calls the chosen function with this URL.

</details>

[^1]: Entering these special characters can be done in Emacs with the command **C-x 8 C-m**, followed by "KEY" or "ROBOT FACE".


## Shortcut Options

### Command Name

While the command name can be any string (see [Symbol Type](https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Type.html)), it is normally easier to choose a consistent pattern. For example, lowercase and hyphenated words follows the style of many commands in Emacs.

### Type of Shortcut

Shortcuts are categorized into two types: URL shortcuts and File shortcuts. These two categories are used simply to group the available functions to apply on a shortcut. For example, a find-file function is responsible for opening a file. Hence, this function belongs to the "File shortcuts" list.

### Functions

Functions determine the action to apply on a path or a URL. These functions are defined by Emacs and are generally ones that can be called interactively. The `Shortcuts` module currently limits the set of functions to a few that address the basic use cases listed above. For example, the function to open a URL in a browser is `browse-url`.

### Specifying parameters in URLs and file paths

A URL or a file path can include any number of parameters.

Parameters define two features for the prompt that the user sees when the command is called:
  * the prompt text the user sees
  * the valid input the user can enter

A parameter can be located anywhere. It must be delimited with one of two unique characters not generally present in a URL or file path. These characters are used by the `Shortcuts` module to validate the input the user can enter. The two valid inputs are strings and lisp expressions. These characters can be changed by updating the corresponding custom variables as shown in the table below.

| Valid User Input | Standard Value          | Custom Variable                   |
|------------------|-------------------------|-----------------------------------|
| string           | 🔑 (KEY 0x1F511)        | link-string-replacement-separator |
| lisp expression | 🤖 (ROBOT FACE 0x1F916) | link-lisp-replacement-separator   |

Between these characters, any text can be entered. This text is used when prompting the user to enter the replacement text at the position where this parameter figures in the URL or file path.

### Lisp expressions as parameters to URLs

Lisp expressions can be used to produce a list of URLs based on a single URL. See [An Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html) for a beginner's introduction to Emacs Lisp.

Lisp expressions should produce a flat list of strings. Each string is used to complete the URL. This results in as many URLs as there are items in the list. Note that values that can be formatted as strings are also valid, as the module will internally format them as strings when completing the URL.

#### Example

In the Wikipedia example from above, the URL could use a Lisp expression for the parameter: `https://en.wikipedia.org/wiki/🤖Article🤖`.

Now, **M-x wikipedia** will prompt you for a Lisp expression. Providing for example `(list "earth" "wind" "fire")` will result in three browser tabs opened for each of "earth", "wind" and "fire".

Note that when a URL includes multiple Lisp parameters, there will be as many URLs opened as there are combinations of values produced by these parameters.

For example, specifying $M$ pull requests for $N$ repositories on github will result in opening $M \times N$ tabs.
  * `https://github.com/🔑Org🔑/🤖Repository🤖/pull/🤖Pull Request Number🤖`
  * Example user inputs are:
    * Org: `github`
    * Repository: ``("copilot-docs" "fetch")`
    * Pull Request Number: `(number-sequence 1 3)`
