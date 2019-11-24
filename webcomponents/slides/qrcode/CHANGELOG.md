<a name="1.0.0-rc.2-1"></a>
# 1.0.0-rc.2-1 (2019-10-12)

### Fix

* don't set a default 100% value as it might be to wide depending on the slide format

<a name="1.0.0-rc.2"></a>
# 1.0.0-rc.2 (2019-10-07)

### Features

* `--slide-qrcode-title-display` default set to `block`
* reflect to attribute thee property `content`
* implement new interface `DeckdeckgoSlideResize`
* add a new property to display a logo over the QR code
* on `componentDidUpdate()` lazy load the logo too
