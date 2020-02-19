# deckgo-slide-aspect-ratio

<!-- Auto Generated Below -->

## Properties

| Property           | Attribute           | Description | Type      | Default  |
| ------------------ | ------------------- | ----------- | --------- | -------- |
| `customActions`    | `custom-actions`    |             | `boolean` | `false`  |
| `customBackground` | `custom-background` |             | `boolean` | `false`  |
| `editable`         | `editable`          |             | `boolean` | `false`  |
| `grid`             | `grid`              |             | `boolean` | `false`  |
| `ratio`            | `ratio`             |             | `number`  | `16 / 9` |

## Events

| Event          | Description | Type                |
| -------------- | ----------- | ------------------- |
| `slideDidLoad` |             | `CustomEvent<void>` |

## Methods

### `afterSwipe() => Promise<void>`

#### Returns

Type: `Promise<void>`

### `beforeSwipe(_enter: boolean, _reveal: boolean) => Promise<boolean>`

#### Returns

Type: `Promise<boolean>`

### `getContainer() => Promise<HTMLDivElement>`

#### Returns

Type: `Promise<HTMLDivElement>`

### `hideContent() => Promise<void>`

#### Returns

Type: `Promise<void>`

### `lazyLoadContent() => Promise<void>`

#### Returns

Type: `Promise<void>`

### `revealContent() => Promise<void>`

#### Returns

Type: `Promise<void>`

---

_Built with [StencilJS](https://stenciljs.com/)_
