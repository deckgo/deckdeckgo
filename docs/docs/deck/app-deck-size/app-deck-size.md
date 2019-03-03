# Size

Per default, the [DeckDeckGo] deck will use all the browser `window` size respectively width and height.

However, it is possible to include or use [DeckDeckGo] in any container, for that purpose you would only need to set the attribute `embedded` to `true`.

```
<div style="width: 500px; height: 400px;">
  <deckgo-deck embedded="true">
    <deckgo-slide-title>
      <h1 slot="title">My presentation title</h1>
      <p slot="content">
        Hello World 🚀
      </p>
    </deckgo-slide-title>
  </deckgo-deck>
</div>
```

[DeckDeckGo]: https://deckdeckgo.com
