# deckgo-youtube



<!-- Auto Generated Below -->


## Properties

| Property          | Attribute          | Description                                                                                                                              | Type      | Default     |
| ----------------- | ------------------ | ---------------------------------------------------------------------------------------------------------------------------------------- | --------- | ----------- |
| `allowFullscreen` | `allow-fullscreen` | Allow option to toggle video in full screen                                                                                              | `boolean` | `true`      |
| `frameTitle`      | `frame-title`      | A title for the frame, could be use for accessibility reason                                                                             | `string`  | `undefined` |
| `height`          | `height`           | The height of the video player                                                                                                           | `number`  | `undefined` |
| `instant`         | `instant`          | In case you would like to load the video as soon as the component is loaded                                                              | `boolean` | `false`     |
| `src`             | `src`              | The source url, the YouTube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by YouTube | `string`  | `undefined` |
| `width`           | `width`            | The width of the video player                                                                                                            | `number`  | `undefined` |


## Methods

### `lazyLoadContent() => Promise<void>`

Lazy load the video

#### Returns

Type: `Promise<void>`



### `pause() => Promise<void>`

Pause the video

#### Returns

Type: `Promise<void>`



### `play() => Promise<void>`

Play the video

#### Returns

Type: `Promise<void>`



### `updateIFrame(width: number, height: number) => Promise<void>`

Update the iFrame, the video, size

#### Returns

Type: `Promise<void>`




## CSS Custom Properties

| Name                                  | Description                                                   |
| ------------------------------------- | ------------------------------------------------------------- |
| `--deckgo-youtube-opacity-loaded`     | The opacity of the video once loaded @default 1               |
| `--deckgo-youtube-opacity-not-loaded` | The opacity of the video when not loaded @default 0           |
| `--deckgo-youtube-transition`         | The transition of the container @default opacity 0.15s linear |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
