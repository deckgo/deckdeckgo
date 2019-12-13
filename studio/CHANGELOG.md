<a name="1.0.0-beta.10"></a>
# 1.0.0-beta.10 (2019-12-04)

### Breaking Changes

* code elements displayed per default as stylish cards with syntax highlighting ([#525](https://github.com/deckgo/deckdeckgo/issues/525))

### Features

* select the transition style of the animation between your slides ([#57](https://github.com/deckgo/deckdeckgo/issues/57))
* improve the accessibility options of the deck by moving the style to a separate popover ([#520](https://github.com/deckgo/deckdeckgo/issues/520)) 
* move color options for the code to the specific color options pane. this unify the UX of the color options ([#394](https://github.com/deckgo/deckdeckgo/issues/394))
* invert order of information and inputs on the voting page to avoid the input being under the keyboard on iOS ([#518](https://github.com/deckgo/deckdeckgo/issues/518)) 

### Fix

* `lazy-img` component wasn't displaying svg anymore ([#532](https://github.com/deckgo/deckdeckgo/pull/532))
* if action are disabled, events on toolbar buttons should be blocked too ([#522](https://github.com/deckgo/deckdeckgo/pull/522))
* code typo as popover are `HTMLIonPopoverElement` ([#523](https://github.com/deckgo/deckdeckgo/pull/523)) 

<a name="1.0.0-beta.9"></a>
# 1.0.0-beta.9 (2019-12-04)

### Features

* interact with your audience with a **live poll** ([#471](interact with your audience))
* dark and light mode theme switcher ([#445](https://github.com/deckgo/deckdeckgo/issues/445))
* hide pager in fullscreen mode on mouse inactivity ([#494](https://github.com/deckgo/deckdeckgo/issues/494))
* size of the video element improved ([#492](https://github.com/deckgo/deckdeckgo/issues/492))
* display smoothly lazy loaded loaded images ([#508](https://github.com/deckgo/deckdeckgo/issues/508))
* improve developer documentation grammar ([#506](https://github.com/deckgo/deckdeckgo/pull/506) | [#505](https://github.com/deckgo/deckdeckgo/pull/505) | [#504](https://github.com/deckgo/deckdeckgo/pull/504))
* improve contributing guide grammar ([#502](https://github.com/deckgo/deckdeckgo/pull/502))
* color picker better highlight ([#488](https://github.com/deckgo/deckdeckgo/issues/488))
* confirm slides delete ([#478](https://github.com/deckgo/deckdeckgo/issues/478))
* add an information page about the remote control ([#477](https://github.com/deckgo/deckdeckgo/pull/477))
* fetch more cards on the main feed (each steps) ([#476](https://github.com/deckgo/deckdeckgo/pull/476))

### Fix

* notes not hidden on template `split` ([#472](https://github.com/deckgo/deckdeckgo/issues/472))
* QR code URL `DECKDECKGO_BASE_HREF` wasn't updated ([#490](https://github.com/deckgo/deckdeckgo/issues/490))
* reset deck background color doesn't work ([#479](https://github.com/deckgo/deckdeckgo/issues/479))
* support click in input type file ([#474](https://github.com/deckgo/deckdeckgo/issues/474))

<a name="1.0.0-beta.8-1"></a>
# 1.0.0-beta.8-1 (2019-11-13)

### Features

* fetch more cards on the main feed ([#476](https://github.com/deckgo/deckdeckgo/pull/476))

### Fix

* fix notes displayed on template split ([#472](https://github.com/deckgo/deckdeckgo/issues/472))
* Firefox and Safari doesn't support click in input type file ([#474](https://github.com/deckgo/deckdeckgo/issues/474))

<a name="1.0.0-beta.8"></a>
# 1.0.0-beta.8 (2019-11-10)

### Features

* add notes to the editor ([#247](https://github.com/deckgo/deckdeckgo/issues/247))
* rework UX and design of the remote controller ([#228](https://github.com/deckgo/deckdeckgo/issues/228))
* sync and display deck and slides content ([#450](https://github.com/deckgo/deckdeckgo/issues/450))
* close remote connect modal on successful connection with the remote ([#464](https://github.com/deckgo/deckdeckgo/pull/464))
* refactor deprecated Ionic controllers ([#454](https://github.com/deckgo/deckdeckgo/issues/454))
* move utils, types and slides-utils to new package utils ([#453](https://github.com/deckgo/deckdeckgo/issues/453))

### Fix

* if modal open, arrow key should not be interpreted ([#443](https://github.com/deckgo/deckdeckgo/issues/443))
* clear draw doesn't clear history ([#452](https://github.com/deckgo/deckdeckgo/issues/452))

<a name="1.0.0-beta.7-1"></a>
# 1.0.0-beta.7-1 (2019-10-21)

### Features

* add split template vertical ([#408](https://github.com/deckgo/deckdeckgo/issues/408))

### Fix

* per-line highlight is wrong unless "display line numbers" is set ([#423](https://github.com/deckgo/deckdeckgo/issues/423))

<a name="1.0.0-beta.7"></a>
# 1.0.0-beta.7 (2019-10-20)

### Features

* integrate template chart ([#400](https://github.com/deckgo/deckdeckgo/issues/400))
* add call to action "create a presentation" ([#411](https://github.com/deckgo/deckdeckgo/issues/411))
* expose studio configuration for contributors ([#413](https://github.com/deckgo/deckdeckgo/pull/413))
* add a dummy press kit page ([#409](https://github.com/deckgo/deckdeckgo/pull/409))
* display tags and hide title on cards of the main feed ([#371](https://github.com/deckgo/deckdeckgo/issues/371))

### Fix

* move slide time to time doesn't work ([#404](https://github.com/deckgo/deckdeckgo/issues/404))

<a name="1.0.0-beta.6"></a>
# 1.0.0-beta.6 (2019-10-07)

### Features

* integrate template QR code ([#384](https://github.com/deckgo/deckdeckgo/issues/384))
* add a meta attribute for the display in the feed ([#361](https://github.com/deckgo/deckdeckgo/issues/361))
* mock api ([#367](https://github.com/deckgo/deckdeckgo/issues/367))
* background opacity and missing "white" in color picker  ([#348](https://github.com/deckgo/deckdeckgo/issues/348))
* randomize feed ([#374](https://github.com/deckgo/deckdeckgo/issues/374))
* don't persist "grammarly" injected data ([#376](https://github.com/deckgo/deckdeckgo/issues/376))

### Fix

* non latin characters as presentation's name / url ([#385](https://github.com/deckgo/deckdeckgo/issues/385))

<a name="1.0.0-beta.5-2"></a>
# 1.0.0-beta.5-2 (2019-09-23)

### Features

* use new color picker for code ([#353](https://github.com/deckgo/deckdeckgo/issues/353))
* improve tags spacing

### Fix

* missing "description" in the published presentation ([#351](https://github.com/deckgo/deckdeckgo/issues/351))
* trim deck name when publishing (as it is use as room name for the remote control)

<a name="1.0.0-beta.5-1"></a>
# 1.0.0-beta.5-1 (2019-09-21)

### Fix

* inline editor color and link on sticky mobile devices

<a name="1.0.0-beta.5"></a>
# 1.0.0-beta.5 (2019-09-21)

### Features

* use a custom color picker to select and apply colors which still offers the platform's picker as extra choice ([#260](https://github.com/deckgo/deckdeckgo/issues/260))
* hide fullscreen and platform's color picker on iPad ([#347](https://github.com/deckgo/deckdeckgo/issues/347))

<a name="1.0.0-beta.4"></a>
# 1.0.0-beta.4 (2019-09-20)

### Breaking Changes

* publish api v2 ([#341](https://github.com/deckgo/deckdeckgo/issues/341))

### Features

* copy slides ([#313](https://github.com/deckgo/deckdeckgo/issues/313))
* add twitter meta information to published presentations ([#332](https://github.com/deckgo/deckdeckgo/issues/332))
* add "dashboard" link in left pane menu ([#330](https://github.com/deckgo/deckdeckgo/pull/330))

### Fix

* firebase not defined on slide delete ([#329](https://github.com/deckgo/deckdeckgo/pull/329))

<a name="1.0.0-beta.3-2"></a>
# 1.0.0-beta.3-2 (2019-09-15)

### Refactoring

* remote event `youtube_pause` renamed to `pause`

<a name="1.0.0-beta.3-1"></a>
# 1.0.0-beta.3-1 (2019-09-09)

### Fix

* workaround to resize `ion-app`
* highlight lines of code in Chrome

<a name="1.0.0-beta.3"></a>
# 1.0.0-beta.3 (2019-09-08)

### Breaking changes

* use uid as storage root folder instead of username (as the username could change) ([#310](https://github.com/deckgo/deckdeckgo/issues/310) and [#319](https://github.com/deckgo/deckdeckgo/issues/319))

### Features

* on the dashboard, add new actions to delete presentations on demand ([#314](https://github.com/deckgo/deckdeckgo/issues/314) and [#322](https://github.com/deckgo/deckdeckgo/issues/322))
* delete decks, slides and storage when user delete his/her account ([#310](https://github.com/deckgo/deckdeckgo/issues/310))
* add a contributing documentation page ([#304](https://github.com/deckgo/deckdeckgo/issues/304))
* in studio and starter, align text center ([#293](https://github.com/deckgo/deckdeckgo/issues/293))
* caret color ([#285](https://github.com/deckgo/deckdeckgo/issues/285))
* improve tags style ([#324](https://github.com/deckgo/deckdeckgo/issues/324))

### Fix

* change username not resolving and clearing its value ([#298](https://github.com/deckgo/deckdeckgo/issues/298))
* reflect slide deletion to the publishing ([#243](https://github.com/deckgo/deckdeckgo/issues/243))
* weird publishing issue ([#289](https://github.com/deckgo/deckdeckgo/issues/289))
* delete header and footer on `<deckgo-slide-gif/>` template ([#307](https://github.com/deckgo/deckdeckgo/issues/307))
* word "hydrated" couldn't be used in a presetnation ([#302](https://github.com/deckgo/deckdeckgo/issues/302))
* twitter url typo ([#297](https://github.com/deckgo/deckdeckgo/issues/297))
* don't persist `deckgo-reveal-list` status ([#295](https://github.com/deckgo/deckdeckgo/pull/295))
* toggle full screen using the keyboard "Escape" key ([#287](https://github.com/deckgo/deckdeckgo/issues/287)) 

<a name="1.0.0-beta.2"></a>
# 1.0.0-beta.2 (2019-08-30)

### Features

* display presentation name instead of "DeckDeckGo beta" ([#248](https://github.com/deckgo/deckdeckgo/issues/248))
* sort deck with "updated_at" ([#249](https://github.com/deckgo/deckdeckgo/issues/249))
* highlight selected element ([#250](https://github.com/deckgo/deckdeckgo/issues/250))
* tagging completes on pause, not on enter ([#256](https://github.com/deckgo/deckdeckgo/issues/256))
* improve images public notice ([#261](https://github.com/deckgo/deckdeckgo/issues/261))
* fullscreen open to presentation mode ([#263](https://github.com/deckgo/deckdeckgo/issues/263))

### Fix

* inline-editor: tools appearing on continuing selection ([#280](https://github.com/deckgo/deckdeckgo/issues/280))
* slide-youtube: if I reload a presentation's youtube page, it's xmas ([#262](https://github.com/deckgo/deckdeckgo/issues/262))
* first slide saved in previous deck ([#254](https://github.com/deckgo/deckdeckgo/issues/254))
* element image converted in code ([#259](https://github.com/deckgo/deckdeckgo/issues/259))

<a name="1.0.0-beta.1"></a>
# 1.0.0-beta.1 (2019-08-30)

* to infinity and beyond 🚀
