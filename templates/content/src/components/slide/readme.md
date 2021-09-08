# deckgo-slide-content

The "Content" slide is a simple slide which display its title and content aligned to the start of the page.

<!-- Auto Generated Below -->


## Properties

| Property           | Attribute           | Description                                                                                             | Type      | Default |
| ------------------ | ------------------- | ------------------------------------------------------------------------------------------------------- | --------- | ------- |
| `customActions`    | `custom-actions`    | If you provide actions for the all deck but, a specific one for this slide, set this option to true     | `boolean` | `false` |
| `customBackground` | `custom-background` | If you define a background for the all deck but, a specific one for this slide, set this option to true | `boolean` | `false` |


## Events

| Event          | Description                        | Type                |
| -------------- | ---------------------------------- | ------------------- |
| `slideDidLoad` | Triggered when the slide is loaded | `CustomEvent<void>` |


## Methods

### `afterSwipe() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `beforeSwipe(enter: boolean, reveal: boolean) => Promise<boolean>`



#### Returns

Type: `Promise<boolean>`



### `hideContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `lazyLoadContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `revealContent() => Promise<void>`



#### Returns

Type: `Promise<void>`




## Slots

| Slot           | Description                        |
| -------------- | ---------------------------------- |
| `"actions"`    | Custom actions for this slide      |
| `"background"` | A custom background for this slide |
| `"content"`    | A content                          |
| `"footer"`     | A custom footer for this slide     |
| `"header"`     | A custom header for this slide     |
| `"notes"`      | Some notes related to this slide   |
| `"title"`      | A title                            |


## CSS Custom Properties

| Name                              | Description                                                        |
| --------------------------------- | ------------------------------------------------------------------ |
| `--background`                    | background                                                         |
| `--color`                         | color                                                              |
| `--overflow`                      | overflow of the slide @default hidden                              |
| `--slide-content-justify-content` | content justify @default flex-start                                |
| `--slide-padding-bottom`          | Padding bottom of the slide @default 64px and 32px on wider screen |
| `--slide-padding-end`             | Padding right of the slide @default 64px and 32px on wider screen  |
| `--slide-padding-start`           | Padding left of the slide @default 64px and 32px on wider screen   |
| `--slide-padding-top`             | Padding top of the slide @default 64px and 32px on wider screen    |
| `--slide-user-select`             | user select @default none                                          |
| `--zIndex`                        | z-index @default 1                                                 |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
