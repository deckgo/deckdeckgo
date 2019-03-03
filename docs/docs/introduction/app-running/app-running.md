# Running

There are two options to run and showcase your presentation:

You could either publish your deck online and showcase it from there, by accessing it with its online url with your favorite browser. For example by accessing an URL like [https://deckdeckgo.com](https://deckdeckgo.com).

Or you could showcase your deck in your favorite browser by running it locally using the integrated dev server provided by the [DeckDeckGo] starter kit with the default access URL [http://localhost:3000](http://localhost:3000).

## Local

To run your presentation, in a terminal, start the following command to bundle your slides and to keep listening to modifications:

```bash
npm run start
```

If you wish to develop your presentation without adding it to the list of available deck of the remote control, run the following command instead:

```bash
npm run start-no-remote
```

> Furthermore to serve your deck, both above commands are watching your presentation's source files for changes and will trigger a new build and reload in case of modifications

[lite-server](https://github.com/johnpapa/lite-server) is use as integrated dev server.

## Online

If you are looking to showcase your presentation from an online URL, have a look to the next chapter [publishing](/docs/publishing) before deploying your deck on your hosting solution.

[DeckDeckGo]: https://deckdeckgo.com 