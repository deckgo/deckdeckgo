# Slide: Countdown

The "Countdown" slide displays a countdown until your presentation starts.

It could be handy, for example when you organize a meetup, to display a countdown until the event start.

## Table of contents

- [Layout](#app-slide-countdown-layout)
- [Usage](#app-slide-countdown-usage)
  - [Usage](#app-slide-countdown-usage-1)
  - [Slots](#app-slide-countdown-slots)
  - [Notes](#app-slide-countdown-notes)
- [Attributes](#app-slide-countdown-attributes)
  - [Example without any slots](#app-slide-countdown-example-without-any-slots)
- [Theming](#app-slide-countdown-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-countdown hours={1} minutes={0} seconds={5}>
        <h1 slot="title">slot="title"</h1>
        <p slot="hours">slot="hours"</p>
        <p slot="minutes">slot="minutes"</p>
        <p slot="seconds">slot="seconds"</p>
    </deckgo-slide-countdown>
  </deckgo-deck>
</div>

## Usage

The "Countdown" slide's Web Component could be integrated using the tag `<deckgo-slide-countdown/>`.

### Usage

```
<deckgo-deck>
    <deckgo-slide-countdown until="2019-08-05T23:25:59.000+02:00">
        <h1 slot="title">My presentation starts at</h1>
        <p slot="days">Days</p>
        <p slot="hours">Hours</p>
        <p slot="minutes">Minutes</p>
        <p slot="seconds">Seconds</p>
    </deckgo-slide-countdown>
</deckgo-deck>  
```

### Slots

The slots `title` as well as `days`, `hours`, `minutes` and `seconds` are optional.

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute `show`.

## Attributes

The time until your presentation should be provided to render the countdown. This value could be either passed through attributes `days`, `hours`, `minutes` and `seconds` or as a particular date using `until`. 

This components exposes the following attributes:
                                                                                                                                                                                                                                                        
| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| days | number | 0 | The amount of days before your presentations (max. 99 will be displayed) |
| hours | number | 0 | The amount of hours before your presentations (max. 23) |
| minutes | number | 0 | The amount of minutes before your presentations (max. 59) |
| seconds | number | 0 | The amount of seconds before your presentations (max. 59) |
| until | string | | A specific date and time until when your presentation will start |
| custom-background | boolean | false | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions | boolean | false | If you would provide actions for the all deck and a specific one for this slide, set this option to `true` |

If you would provide a date using `until`, note that the format should be provided as a valid and parsable date. See [Date.parse()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date/parse) for more information about the format.

### Example without any slots

```
<deckgo-deck>
  <deckgo-slide-qrcode hours="0" minutes="10" seconds="45">
  </deckgo-slide-code>
</deckgo-deck>  
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --slide-countdown-container-padding-bottom | 64px | The bottom padding of the displayed time container |
| --slide-countdown-digits-max-width  | 36em | The max width of the container containing all digits |
| --slide-countdown-digits-minmax-width  | 12em | The grid minmax value of one of the three columns of digits |
| --slide-countdown-digits-width  | 4em | The width of one of the (two) digits |
| --slide-countdown-digits-height | 4m | The height of the (two) digits |
| --slide-countdown-digits-background |  | The background color of the (two) digits |
| --slide-countdown-digits-border-radius |  | The border-radius of the (two) digits |
| --slide-countdown-digits-box-shadow | 0 3px 4px 0 rgba(0, 0, 0, .2), inset 2px 4px 0 0 rgba(255, 255, 255, .08) | The box-shadow of the (two) digits |
| --slide-countdown-digit-margin-right | 0.625em | The space between two digits |
| --slide-countdown-digits-font-size | 3em | The border-radius of the (two) digits |
| --slide-countdown-digits-font-weight |  | The font-weight of the (two) digits |
| --slide-countdown-digits-color |  | The color of the (two) digits |

[DeckDeckGo]: https://deckdeckgo.com