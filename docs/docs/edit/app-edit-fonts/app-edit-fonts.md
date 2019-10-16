# Fonts

As any web application, the fonts of your presentation could be easily styled, but [DeckDeckGo] goes one step further by downloading and installing **automatically** any [Google Fonts](https://fonts.google.com) you would like to use during the setup process (if you are using the starter kit). 

## Using automatically any Google Fonts

After you have kick-started your presentation running `npm init deckdeckgo` in a terminal and provided the information as describe in the [Getting started](https://docs.deckdeckgo.com/docs/introduction) chapter, the [DeckDeckGo] CLI will ask you if you are looking to use Google Fonts and if you would answer yes, will guide you to get some information about the font.

Once all information gathered, it will automatically download the fonts, add them to your presentation and even do the settings in the CSS stylesheets for you.

## Example

The following is an example of the process if you would like to install the Google Font `Lato`:

```
? Do you want to use a Google Font for your presentation? (y/N) Y

⠼ Fetching fonts list...

? Search a Google font (min. 3 characters)? ato

? Select the font (Use arrow keys)
  Atomic Age 
❯ Lato 
  Search again 
  Skip 
  
? Select charsets (Press <space> to select, <a> to toggle all, <i> to invert selection)
❯◉ latin
 ◯ latin-ext
 
? Select styles (Press <space> to select, <a> to toggle all, <i> to invert selection)
  ◯ 100
  ◯ 100italic
  ◯ 300
  ◯ 300italic
 ❯◉ regular
  ◯ italic
  ◯ 700
 (Move up and down to reveal more choices)
 
⠼ Downloading font
⠼ Writing to CSS files...

``` 

## Video

Have a look at this video where we show how to do it!

<iframe width="560" height="315" src="https://www.youtube.com/embed/S6qL7JbxJ70" frameborder="0"></iframe>

[DeckDeckGo]: https://deckdeckgo.com