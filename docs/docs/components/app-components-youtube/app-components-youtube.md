# Youtube

The "Youtube" component allows you to easily add a [Youtube](https://youtube.com) video in almost any slide of your presentation.

## Table of contents

- [Showcase](#app-components-youtube-showcase)
- [Usage](#app-components-youtube-usage)
  - [Slots](#app-components-youtube-slots)
  - [Attributes](#app-components-youtube-attributes)

## Showcase

<div>
  <deckgo-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw" width={300} height={200}>
  </deckgo-youtube>
</div>

## Usage

The "Youtube" slide's Web Component could be integrated using the tag `<deckgo-youtube/>`.

```
<deckgo-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
</deckgo-youtube>
```

### Slots

No slots are available for this component.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| src | string |  | The source url, the Youtube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by Youtube. |
| width | number |  | The width of the video player. |
| height | number |  | The height of the video player. |
| frame-title | string |  | A title for the frame, could be use for accessibility reason. | 

[DeckDeckGo]: https://deckdeckgo.com 