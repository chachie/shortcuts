# Customizable URL & file shortcuts in Emacs

The `Shortcuts` module allows you to define commands in Emacs to quickly access webpages and files.

## Use cases

  * Opening one or multiple distinct URLs with a single command
    * Opening a single URL is the equivalent of a browser bookmark
  * Opening one or multiple webpages from a single [parametrized URL](https://developer.mozilla.org/en-US/docs/Learn/Common_questions/Web_mechanics/What_is_a_URL#parameters)
    * The URL is completed by prompting the user for the missing parameters
  * Copying the URL(s) produced by the above commands (instead of opening)
    * Used for example when sharing the URL(s) in an email or document

The use cases above are also applicable when opening files instead of URLs.

## Example

Let's say you would like to navigate to the Wikipedia article about Emacs.

* Place the `shortcuts.el` in your load path.
* **M-x customize-option**
* Type `shortcuts-list`.
* Under `Shorcuts List`, hit `INS` to add a new shortcut.
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
