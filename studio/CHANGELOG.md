# 4.5.0 (2020-04-03)

### Features

- figma integration ([#246](https://github.com/deckgo/deckdeckgo/issues/246))
- german translations ([#1122](https://github.com/deckgo/deckdeckgo/issues/1122) and [#1123](https://github.com/deckgo/deckdeckgo/issues/1123))
- accept images svg and webp ([#1113](https://github.com/deckgo/deckdeckgo/pull/1113)) 
- csp object-src and base-uri ([#1111](https://github.com/deckgo/deckdeckgo/pull/1111))

# 4.4.0 (2020-03-18)

### Features

- spanish translations ([#360](https://github.com/deckgo/deckdeckgo/issues/360))

### Fix

- unable to go offline ([#1104](https://github.com/deckgo/deckdeckgo/issues/1104))
- notes encoding text incorrectly ([#1105](https://github.com/deckgo/deckdeckgo/issues/1105))

# 4.3.0 (2020-03-12)

### Features

- i18n ([#370](https://github.com/deckgo/deckdeckgo/issues/370))
- toggle slide type on the fly ([#251](https://github.com/deckgo/deckdeckgo/issues/251))
- do not fetch code language list at runtime to improve performance ([#1083](https://github.com/deckgo/deckdeckgo/issues/1083))
- add telegram to web-social-share options ([#1090](https://github.com/deckgo/deckdeckgo/issues/1090))

### Fix

- notes not persisted - listener has to be attached higher ([#1102](https://github.com/deckgo/deckdeckgo/pull/1102))
- support swipe for templates with custom names ([#1079](https://github.com/deckgo/deckdeckgo/pull/1079))
- custom templates names ([#1086](https://github.com/deckgo/deckdeckgo/pull/1086))
- main deck selector to go offline ([#1088](https://github.com/deckgo/deckdeckgo/pull/1088))
- low contrast warning ([#1094](https://github.com/deckgo/deckdeckgo/pull/1094))
- publish action disable state ([#1095](https://github.com/deckgo/deckdeckgo/issues/1095))

# 4.2.0 (2020-02-10)

### Features

- user and community templates ([#1031](https://github.com/deckgo/deckdeckgo/pull/1031) and [#1056](https://github.com/deckgo/deckdeckgo/pull/1056))
- new default template with content aligned at the bottom of the slide ([#1073](https://github.com/deckgo/deckdeckgo/pull/1073))
- edit with default tool or with CSS ([#1066](https://github.com/deckgo/deckdeckgo/pull/1066))
- copy and paste elements style ("format painter") ([#1071](https://github.com/deckgo/deckdeckgo/pull/1071))
- width, rotate and padding options for elements ([#1070](https://github.com/deckgo/deckdeckgo/pull/1070))
- keep track of panel state open or close ([#1065](https://github.com/deckgo/deckdeckgo/pull/1065))
- minimum six slides, instead of four, to appear in "discover" section ([#1068](https://github.com/deckgo/deckdeckgo/pull/1068))
- handle assets with store ([#1065](https://github.com/deckgo/deckdeckgo/pull/1062))

### Style

- new design of the editor's tools ([#1069](https://github.com/deckgo/deckdeckgo/pull/1069))

### Fix

- preview code block update ([#1067](https://github.com/deckgo/deckdeckgo/pull/1067))
- navigate contact ([#1055](https://github.com/deckgo/deckdeckgo/pull/1055))

# 4.1.0 (2020-01-14)

### Features

- add option to not display the toolbar on code block
- highlight code editing less quirky
- do not supercharge paste action anymore

### Fix

- undefined reference in preview

### Refactor

- rename files with `.ts` instead of `.tsx`

# 4.0.0 (2020-12-23)

### Features

- remove landing, discover, feed and all static pages from studio to split the site (landing) and editor (app) in two separate applications

# 3.2.1 (2020-12-16)

### Features

- disable spellcheck in fullscreen ([#264](https://github.com/deckgo/deckdeckgo/issues/264))
- split settings in two routes customization and profile ([#1028](https://github.com/deckgo/deckdeckgo/pull/1028))
- goodfirms featured story ([#1033](https://github.com/deckgo/deckdeckgo/pull/1033))
- avatar display and size in menu ([#1029](https://github.com/deckgo/deckdeckgo/pull/1029))
- display brand name in navigation offline ([#1025](https://github.com/deckgo/deckdeckgo/pull/1025))
- update robots.txt ([#1030](https://github.com/deckgo/deckdeckgo/issues/1030))

# 3.2.0 (2020-11-28)

### Features

- fullscreen and remote together under a new "present" panel ([#1020](https://github.com/deckgo/deckdeckgo/pull/1020))
- offline: download backup ([#1015](https://github.com/deckgo/deckdeckgo/issues/1015))

### Fix

- offline: header and footer if not defined
- offline: firestore delete fields comparison

# 3.1.0 (2020-11-22)

### Features

- markdown ([#996](https://github.com/deckgo/deckdeckgo/issues/996))
- preview slide when editing markdown, code and math ([#1013](https://github.com/deckgo/deckdeckgo/pull/1013))
- backup firestore once a day ([#1016](https://github.com/deckgo/deckdeckgo/pull/1016))
- optionally skip second element in create slide title or content ([#1010](https://github.com/deckgo/deckdeckgo/pull/1010))
- text made selectable in published slides ([#1008](https://github.com/deckgo/deckdeckgo/pull/1008), [#1006](https://github.com/deckgo/deckdeckgo/pull/1006) and [#1009](https://github.com/deckgo/deckdeckgo/pull/1009))
- allow any https images ([#1004](https://github.com/deckgo/deckdeckgo/pull/1004))
- improve img lazy loading ([#1000](https://github.com/deckgo/deckdeckgo/pull/1000))
- improve font color focus state ([#999](https://github.com/deckgo/deckdeckgo/pull/999))
- colors in studio do not show focus outline ([#997](https://github.com/deckgo/deckdeckgo/issues/997))
- display information if publish failed ([#989](https://github.com/deckgo/deckdeckgo/pull/989))

### Fix

- offline user was not able to go back online ([#1014](https://github.com/deckgo/deckdeckgo/issues/1014))

# 3.0.0 (2020-10-29)

### Features

- papyrus and vertical ([#871](https://github.com/deckgo/deckdeckgo/issues/871))
- waves ([#874](https://github.com/deckgo/deckdeckgo/issues/874))
- word cloud ([#696](https://github.com/deckgo/deckdeckgo/issues/696))
- box shadow ([#881](https://github.com/deckgo/deckdeckgo/issues/881))
- letter spacing ([#888](https://github.com/deckgo/deckdeckgo/issues/888))
- border radius ([#865](https://github.com/deckgo/deckdeckgo/issues/865))
- auto slide ([#890](https://github.com/deckgo/deckdeckgo/issues/890))
- more list style ([#705](https://github.com/deckgo/deckdeckgo/issues/705))
- aspect ratio set to 16/9 (except on mobile) ([#967](https://github.com/deckgo/deckdeckgo/pull/967))
- improve poll UX to support multiple polls within the same presentation ([#942](https://github.com/deckgo/deckdeckgo/issues/942))
- animate style options of revealed elements ([#236](https://github.com/deckgo/deckdeckgo/issues/236))
- fullscreen enter hide mouse more quickly ([#963](https://github.com/deckgo/deckdeckgo/pull/963))
- fullscreen editor v2 ([#901](https://github.com/deckgo/deckdeckgo/issues/901))
- color picker v2 ([#919](https://github.com/deckgo/deckdeckgo/issues/919))
- save used colors as history ([#902](https://github.com/deckgo/deckdeckgo/issues/902))
- display regions in services list ([#856](https://github.com/deckgo/deckdeckgo/issues/856))
- lighthouse metrics and transition fade ([#877](https://github.com/deckgo/deckdeckgo/issues/877))

### Fix

- some non descriptive icon titles are shown when hovering toolbar options ([#822](https://github.com/deckgo/deckdeckgo/issues/822))
- color picker does not show focus state ([#922](https://github.com/deckgo/deckdeckgo/issues/922))
- missing CSS overflow hidden in list of fonts ([#882](https://github.com/deckgo/deckdeckgo/issues/882))
- anonymous user can add up to three slides to give a try to the editor ([#878](https://github.com/deckgo/deckdeckgo/issues/878))
- improve performance by better detecting all slides effectively loaded ([#884](https://github.com/deckgo/deckdeckgo/issues/884))

### Build

- Workbox v5 ([#691](https://github.com/deckgo/deckdeckgo/issues/691))
- Firebase JavaScript v8 ([#976](https://github.com/deckgo/deckdeckgo/pull/976))
- rm www before build ([#879](https://github.com/deckgo/deckdeckgo/issues/879))

### Refactor

- optional chaining ([#920](https://github.com/deckgo/deckdeckgo/issues/920))
- add rel to links ([#940](https://github.com/deckgo/deckdeckgo/pull/940))

# 2.3.0 (2020-09-06)

### Features

- publish presentations source code to GitHub ([#851](https://github.com/deckgo/deckdeckgo/pull/851))
- highlight why "publish" action would be disabled ([#613](https://github.com/deckgo/deckdeckgo/issues/613))
- new staging environment ([#850](https://github.com/deckgo/deckdeckgo/pull/850))
- make lighthouse happy ([#847](https://github.com/deckgo/deckdeckgo/pull/847))
- update dependencies including all Web Components build with Stencil v2 ([#858](https://github.com/deckgo/deckdeckgo/issues/858))
- upgrade to Stencil v2 ([#858](https://github.com/deckgo/deckdeckgo/issues/858))
- prompt user to reload when an update is available ([#868](https://github.com/deckgo/deckdeckgo/pull/868))

### Style

- less christmassy landing drawings ([#855](https://github.com/deckgo/deckdeckgo/pull/855))

### Fix

- hide placeholder in fullscreen mode if idle ([#854](https://github.com/deckgo/deckdeckgo/pull/854))
- update `@deckdeckgo/highlight-code` ([#848](https://github.com/deckgo/deckdeckgo/issues/848))
- API crash on unique constraint ([#858](https://github.com/deckgo/deckdeckgo/issues/859))

# 2.2.0 (2020-08-17)

### Features

- add warning when colors do not meet contrast ratio (a11y, [#819](https://github.com/deckgo/deckdeckgo/issues/819))
- custom inline editor execCommand implementation ([#741](https://github.com/deckgo/deckdeckgo/issues/741))
- share on feed page and add twitter tags ([#824](https://github.com/deckgo/deckdeckgo/pull/824))
- improve web caching ([#840](https://github.com/deckgo/deckdeckgo/pull/840))
- hide the pager ([#835](https://github.com/deckgo/deckdeckgo/pull/835))
- only one h1 on the landing page (Google SEO friendly, [#834](https://github.com/deckgo/deckdeckgo/pull/834))
- display a message in the console when cannot be use in Firefox Private Window ([#827](https://github.com/deckgo/deckdeckgo/issues/827))
- update dependencies and firebase ui ([#829](https://github.com/deckgo/deckdeckgo/pull/829))
- remove old ionic / chrome resize issue workaround ([#327](https://github.com/deckgo/deckdeckgo/issues/327))

### Fix

- toolbar options skips some options (a11y, [#811](https://github.com/deckgo/deckdeckgo/issues/821))
- unhandled and unexpected add slide ([#830](https://github.com/deckgo/deckdeckgo/pull/830))
- adjust copy enterprise ([#823](https://github.com/deckgo/deckdeckgo/pull/823))

# 2.1.0 (2020-07-30)

### Features

- enterprise: new dedicated page, improved navigation and press update ([#820](https://github.com/deckgo/deckdeckgo/pull/820))

### Fix

- social icons are not shown ([#814](https://github.com/deckgo/deckdeckgo/issues/814))
- add rel to outgoing links for security reasons ([#815](https://github.com/deckgo/deckdeckgo/issues/815))
- aria-labels are incorrectly implemented (typo) ([#812](https://github.com/deckgo/deckdeckgo/issues/812))

# 2.0.0 (2020-07-23)

### Features

- header and footer ([#794](https://github.com/deckgo/deckdeckgo/pull/794), [#791](https://github.com/deckgo/deckdeckgo/pull/791), [#793](https://github.com/deckgo/deckdeckgo/pull/793), [#792](https://github.com/deckgo/deckdeckgo/pull/792))
- floating editable text for diagrams ([#799](https://github.com/deckgo/deckdeckgo/pull/799))
- background color options for selected text ([#805](https://github.com/deckgo/deckdeckgo/pull/805))
- self host Google Fonts to improve load time ([#796](https://github.com/deckgo/deckdeckgo/pull/796))
- shorten default full url displayed social text ([#789](https://github.com/deckgo/deckdeckgo/pull/789))

### Style

- box popover options to improve readability ([#790](https://github.com/deckgo/deckdeckgo/pull/790), [#786](https://github.com/deckgo/deckdeckgo/pull/786))
- highlight code scrollbar (overflow-y) set to auto instead of scroll ([#781](https://github.com/deckgo/deckdeckgo/pull/781))

### Fix

- re-hydrate editor with prerendering ([#797](https://github.com/deckgo/deckdeckgo/pull/797))
- recursively clean Firestore FieldValue.delete ([#807](https://github.com/deckgo/deckdeckgo/pull/807) and [#806](https://github.com/deckgo/deckdeckgo/pull/806))
- improve dashboard navigation and style ([#788](https://github.com/deckgo/deckdeckgo/pull/788))
- use user store to create author slide ([#787](https://github.com/deckgo/deckdeckgo/pull/787))
- code editable attribute was sometimes missing ([#784](https://github.com/deckgo/deckdeckgo/pull/784))

<a name="1.5.0"></a>

# 1.5.0 (2020-07-02)

### Features

- use @stencil/store instead of rxjs ([#773](https://github.com/deckgo/deckdeckgo/issues/773))
- web app shortcuts ([#766](https://github.com/deckgo/deckdeckgo/pull/766))
- use Google Font "Open Sans" for the editor ([#772](https://github.com/deckgo/deckdeckgo/pull/772))
- at least four slides needed to make a published deck appear in feed ([#776](https://github.com/deckgo/deckdeckgo/pull/776))
- remove random color and background when creating a new deck ([#777](https://github.com/deckgo/deckdeckgo/pull/777))
- wider deck style menu popover ([#778](https://github.com/deckgo/deckdeckgo/pull/778))

### Fix

- grammar mistake on app-landing-deck.tsx ([#775](https://github.com/deckgo/deckdeckgo/pull/775))

<a name="1.4.0"></a>

# 1.4.0 (2020-06-25)

### Features

- support copepen, jsfiddle and webcomponents.dev ([#721](https://github.com/deckgo/deckdeckgo/issues/721))
- refresh style of the actions in full screen mode ([#755](https://github.com/deckgo/deckdeckgo/pull/755))
- group, improve fonts in deck style and keep popover open on editing ([#745](https://github.com/deckgo/deckdeckgo/pull/745))
- add strikethrough option ([#710](https://github.com/deckgo/deckdeckgo/issues/710))
- illustrations for the landing page ([#764](https://github.com/deckgo/deckdeckgo/pull/764))
- highlight code line numbers background and color ([#763](https://github.com/deckgo/deckdeckgo/pull/763))
- improve deck li style ([#761](https://github.com/deckgo/deckdeckgo/pull/761))

### Fix

- prerendering 🚀 ([#675](https://github.com/deckgo/deckdeckgo/issues/675))
- share doesn't work in Firefox ([#751](https://github.com/deckgo/deckdeckgo/issues/751))
- problem with remote control layout in Firefox Mobile ([#752](https://github.com/deckgo/deckdeckgo/issues/752))
- QR code sizing/spacing ([#743](https://github.com/deckgo/deckdeckgo/issues/743))
- social card image not generated ([#659](https://github.com/deckgo/deckdeckgo/issues/659))

<a name="1.3.0"></a>

# 1.3.0 (2020-05-28)

### Features

- theming for the carbon terminal to showcase your code ([#733](https://github.com/deckgo/deckdeckgo/issues/733))
- select as popover for a quicker access ([#738](https://github.com/deckgo/deckdeckgo/pull/738))

### UX

- improve onboarding by adding a mini wizard to the create slide process ([#729](https://github.com/deckgo/deckdeckgo/pull/729))
- bring all actions together in two distinct groups: style or options ([#742](https://github.com/deckgo/deckdeckgo/pull/742) and [#744](https://github.com/deckgo/deckdeckgo/pull/744))

### Fix

- support offline access to the editor directly with the browser's url ([#734](https://github.com/deckgo/deckdeckgo/pull/734))
- handle background Firestore Field.delete when going offline ([#735](https://github.com/deckgo/deckdeckgo/pull/735))
- Chrome requires two clicks to focus element if pseudo CSS is being used ([#739](https://github.com/deckgo/deckdeckgo/pull/739))

<a name="1.2.0"></a>

# 1.2.0 (2020-05-15)

### Features

- integration `math` component ([#622](https://github.com/deckgo/deckdeckgo/issues/622))
- colors should have a name description (a11y, [#544](https://github.com/deckgo/deckdeckgo/issues/544))
- replace "toggle" with "transform" ([#731](https://github.com/deckgo/deckdeckgo/pull/731))

### Fix

- preserve card and image ratio on feed ([#727](https://github.com/deckgo/deckdeckgo/pull/727))

<a name="1.1.0"></a>

# 1.1.0 (2020-05-08)

### Features

- showcase interacted apps and websites in presentations ([#715](https://github.com/deckgo/deckdeckgo/issues/715))
- add "Studio" as a PWA to Google Play ([#687](https://github.com/deckgo/deckdeckgo/issues/687))
- deck font review ([#713](https://github.com/deckgo/deckdeckgo/pull/713))
- `font-weight: 700` for h1 and h2 ([#704](https://github.com/deckgo/deckdeckgo/issues/704))

### Fix

- load images and charts in dashboard ([#717](https://github.com/deckgo/deckdeckgo/pull/717))
- alignment fonts in split template ([#716](https://github.com/deckgo/deckdeckgo/pull/716))

<a name="1.0.0"></a>

# 1.0.0 (2020-04-21)

To infinity and beyond 🚀

### Features

- offline mode ([#653](https://github.com/deckgo/deckdeckgo/issues/653))
- four new Google Fonts available ([#660](https://github.com/deckgo/deckdeckgo/issues/660))
- grand access to presentation through remote control ([#67](https://github.com/deckgo/deckdeckgo/issues/67))
- add option to align left/center/right ([#702](https://github.com/deckgo/deckdeckgo/pull/702))
- list ordered and unordered toggle to match new UX (#[703](https://github.com/deckgo/deckdeckgo/pull/703))
- template "split" vertical text alignment ([#679](https://github.com/deckgo/deckdeckgo/issues/679))
- `text-transform: none;` for all buttons ([#665](https://github.com/deckgo/deckdeckgo/issues/665))
- update about page ([#674](https://github.com/deckgo/deckdeckgo/pull/674))

### Fix

- sign in button not accessible via keyboard only on settings page ([#673](https://github.com/deckgo/deckdeckgo/issues/673))
- two H1 headings on settings page ([#671](https://github.com/deckgo/deckdeckgo/issues/671))

<a name="1.0.0-beta.12-1"></a>

# 1.0.0-beta.12-1 (2020-03-09)

### Fix

- rotation miscalculated on first click (drag, drop and resize)
- spacing around social logo on the "author" template

<a name="1.0.0-beta.12"></a>

# 1.0.0-beta.12 (2020-02-27)

### Features

- new template to "draw" schemes with shapes and images while maintaining the same aspect ratio across devices ([#610](https://github.com/deckgo/deckdeckgo/issues/610))
- full rethinking of the accessibility of the editor actions to improve the understanding of the `deck > slide > element` concept ([#612](https://github.com/deckgo/deckdeckgo/issues/612))
- navigation bar design improvements ([#646](https://github.com/deckgo/deckdeckgo/pull/646) and [#616](https://github.com/deckgo/deckdeckgo/issues/616))
- drag arrows over the presentation through the remote control ([#625](https://github.com/deckgo/deckdeckgo/issues/625))
- highlight code optionally displayed in an Ubuntu Terminal ([#623](https://github.com/deckgo/deckdeckgo/issues/623))
- new design for the templates' showcase ([#636](https://github.com/deckgo/deckdeckgo/pull/636) and [#647](https://github.com/deckgo/deckdeckgo/pull/647))
- remove editor popover backdrops ([#635](https://github.com/deckgo/deckdeckgo/pull/635))
- remove label "Beta" in navigation bar ([#645](https://github.com/deckgo/deckdeckgo/pull/645))
- search bar for code languages ([#389](https://github.com/deckgo/deckdeckgo/issues/389))
- add prettier to all components, apps and utils ([#470](https://github.com/deckgo/deckdeckgo/issues/470))
- tag info may be updated ([#603](https://github.com/deckgo/deckdeckgo/issues/603))
- new variable box-shadow for lazy-img component ([#606](https://github.com/deckgo/deckdeckgo/pull/606))
- add social share to Hacker News ([#618](https://github.com/deckgo/deckdeckgo/issues/618))
- Ionic v5, Ionicons v5 and color scheme ([#614](https://github.com/deckgo/deckdeckgo/issues/614))
- style the popover to select slide in the starter kit ([#493](https://github.com/deckgo/deckdeckgo/issues/493))
- add a small box-shadow around the edited deck ([#642(https://github.com/deckgo/deckdeckgo/pull/642)

### Fix

- jump to slide was prevented by our CSP ([#609](https://github.com/deckgo/deckdeckgo/issues/609))
- sign in button not reachable with keyboard only ([#534](https://github.com/deckgo/deckdeckgo/issues/534))
- missing style for editor popover info in code color ([#633](https://github.com/deckgo/deckdeckgo/pull/633))

<a name="1.0.0-beta.11"></a>

# 1.0.0-beta.11 (2020-01-20)

### Features

- Google Fonts for the presentations ([#245](https://github.com/deckgo/deckdeckgo/issues/245))
- private assets ([#585](https://github.com/deckgo/deckdeckgo/issues/585))
- clone decks ([#582](https://github.com/deckgo/deckdeckgo/issues/582))
- landing page ([#561](https://github.com/deckgo/deckdeckgo/issues/561))
- share and embed presentations anywhere ([#359](https://github.com/deckgo/deckdeckgo/issues/359))
- prevent javascript in input field with CSP rules ([#497](https://github.com/deckgo/deckdeckgo/issues/497))
- in db, delete deck attributes instead of keep null values ([#587](https://github.com/deckgo/deckdeckgo/issues/587))
- improve Youtube embed for play and pause ([#569](https://github.com/deckgo/deckdeckgo/pull/569))
- load studio with the background color set to the color once loaded ([#567](https://github.com/deckgo/deckdeckgo/pull/567))
- extract component `<deckgo-youtube/>` ([#565](https://github.com/deckgo/deckdeckgo/issues/565))
- add new design options to slide "author" ([#558](https://github.com/deckgo/deckdeckgo/issues/558))
- show handles in author slide ([#424](https://github.com/deckgo/deckdeckgo/issues/424))
- add canvas desynchronized option ([#437](https://github.com/deckgo/deckdeckgo/issues/437))
- ping on dack published ([#547](https://github.com/deckgo/deckdeckgo/pull/547))

### Accessibility

- toolbar options have no focus state ([#536](https://github.com/deckgo/deckdeckgo/issues/536))
- swipe slide with tabs ([#581](https://github.com/deckgo/deckdeckgo/issues/581))
- main menu has not enough contrast ([#554](https://github.com/deckgo/deckdeckgo/issues/554))
- remote control text is not readable ([#545](https://github.com/deckgo/deckdeckgo/issues/545))
- add issue templates for issues and pull requests ([#533](https://github.com/deckgo/deckdeckgo/issues/533) and [#546](https://github.com/deckgo/deckdeckgo/pull/546))
- presentations should have better naming ([#535](https://github.com/deckgo/deckdeckgo/issues/535))
- remove beta from urls ([#542](https://github.com/deckgo/deckdeckgo/pull/542))
- links should have different styling (to distinguish from emphasised text) ([#537](https://github.com/deckgo/deckdeckgo/issues/537))

### Fix

- remove list of presentations from side menu ([#588](https://github.com/deckgo/deckdeckgo/issues/588))
- navigation button padding-start incorrect ([#584](https://github.com/deckgo/deckdeckgo/issues/584))
- add poll question to list of slides on "jump to slide" ([#598](https://github.com/deckgo/deckdeckgo/issues/598))
- replace all platform icons with local icons ([#563](https://github.com/deckgo/deckdeckgo/issues/563))
- svg are't lazy loaded in browser which support native loading ([#575](https://github.com/deckgo/deckdeckgo/issues/575))
- screenshot not generated anymore ([#548](https://github.com/deckgo/deckdeckgo/issues/548))
- correct typo noscript ([#539](https://github.com/deckgo/deckdeckgo/pull/539))

<a name="1.0.0-beta.10"></a>

# 1.0.0-beta.10 (2019-12-04)

### Breaking Changes

- code elements displayed per default as stylish cards with syntax highlighting ([#525](https://github.com/deckgo/deckdeckgo/issues/525))

### Features

- select the transition style of the animation between your slides ([#57](https://github.com/deckgo/deckdeckgo/issues/57))
- improve the accessibility options of the deck by moving the style to a separate popover ([#520](https://github.com/deckgo/deckdeckgo/issues/520))
- move color options for the code to the specific color options pane. this unify the UX of the color options ([#394](https://github.com/deckgo/deckdeckgo/issues/394))
- invert order of information and inputs on the voting page to avoid the input being under the keyboard on iOS ([#518](https://github.com/deckgo/deckdeckgo/issues/518))

### Fix

- `lazy-img` component wasn't displaying svg anymore ([#532](https://github.com/deckgo/deckdeckgo/pull/532))
- if action are disabled, events on toolbar buttons should be blocked too ([#522](https://github.com/deckgo/deckdeckgo/pull/522))
- code typo as popover are `HTMLIonPopoverElement` ([#523](https://github.com/deckgo/deckdeckgo/pull/523))

<a name="1.0.0-beta.9"></a>

# 1.0.0-beta.9 (2019-12-04)

### Features

- interact with your audience with a **live poll** ([#471](interact with your audience))
- dark and light mode theme switcher ([#445](https://github.com/deckgo/deckdeckgo/issues/445))
- hide pager in fullscreen mode on mouse inactivity ([#494](https://github.com/deckgo/deckdeckgo/issues/494))
- size of the video element improved ([#492](https://github.com/deckgo/deckdeckgo/issues/492))
- display smoothly lazy loaded loaded images ([#508](https://github.com/deckgo/deckdeckgo/issues/508))
- improve developer documentation grammar ([#506](https://github.com/deckgo/deckdeckgo/pull/506) | [#505](https://github.com/deckgo/deckdeckgo/pull/505) | [#504](https://github.com/deckgo/deckdeckgo/pull/504))
- improve contributing guide grammar ([#502](https://github.com/deckgo/deckdeckgo/pull/502))
- color picker better highlight ([#488](https://github.com/deckgo/deckdeckgo/issues/488))
- confirm slides delete ([#478](https://github.com/deckgo/deckdeckgo/issues/478))
- add an information page about the remote control ([#477](https://github.com/deckgo/deckdeckgo/pull/477))
- fetch more cards on the main feed (each steps) ([#476](https://github.com/deckgo/deckdeckgo/pull/476))

### Fix

- notes not hidden on template `split` ([#472](https://github.com/deckgo/deckdeckgo/issues/472))
- QR code URL `DECKDECKGO_BASE_HREF` wasn't updated ([#490](https://github.com/deckgo/deckdeckgo/issues/490))
- reset deck background color doesn't work ([#479](https://github.com/deckgo/deckdeckgo/issues/479))
- support click in input type file ([#474](https://github.com/deckgo/deckdeckgo/issues/474))

<a name="1.0.0-beta.8-1"></a>

# 1.0.0-beta.8-1 (2019-11-13)

### Features

- fetch more cards on the main feed ([#476](https://github.com/deckgo/deckdeckgo/pull/476))

### Fix

- fix notes displayed on template split ([#472](https://github.com/deckgo/deckdeckgo/issues/472))
- Firefox and Safari doesn't support click in input type file ([#474](https://github.com/deckgo/deckdeckgo/issues/474))

<a name="1.0.0-beta.8"></a>

# 1.0.0-beta.8 (2019-11-10)

### Features

- add notes to the editor ([#247](https://github.com/deckgo/deckdeckgo/issues/247))
- rework UX and design of the remote controller ([#228](https://github.com/deckgo/deckdeckgo/issues/228))
- sync and display deck and slides content ([#450](https://github.com/deckgo/deckdeckgo/issues/450))
- close remote connect modal on successful connection with the remote ([#464](https://github.com/deckgo/deckdeckgo/pull/464))
- refactor deprecated Ionic controllers ([#454](https://github.com/deckgo/deckdeckgo/issues/454))
- move utils, types and slides-utils to new package utils ([#453](https://github.com/deckgo/deckdeckgo/issues/453))

### Fix

- if modal open, arrow key should not be interpreted ([#443](https://github.com/deckgo/deckdeckgo/issues/443))
- clear draw doesn't clear history ([#452](https://github.com/deckgo/deckdeckgo/issues/452))

<a name="1.0.0-beta.7-1"></a>

# 1.0.0-beta.7-1 (2019-10-21)

### Features

- add split template vertical ([#408](https://github.com/deckgo/deckdeckgo/issues/408))

### Fix

- per-line highlight is wrong unless "display line numbers" is set ([#423](https://github.com/deckgo/deckdeckgo/issues/423))

<a name="1.0.0-beta.7"></a>

# 1.0.0-beta.7 (2019-10-20)

### Features

- integrate template chart ([#400](https://github.com/deckgo/deckdeckgo/issues/400))
- add call to action "create a presentation" ([#411](https://github.com/deckgo/deckdeckgo/issues/411))
- expose studio configuration for contributors ([#413](https://github.com/deckgo/deckdeckgo/pull/413))
- add a dummy press kit page ([#409](https://github.com/deckgo/deckdeckgo/pull/409))
- display tags and hide title on cards of the main feed ([#371](https://github.com/deckgo/deckdeckgo/issues/371))

### Fix

- move slide time to time doesn't work ([#404](https://github.com/deckgo/deckdeckgo/issues/404))

<a name="1.0.0-beta.6"></a>

# 1.0.0-beta.6 (2019-10-07)

### Features

- integrate template QR code ([#384](https://github.com/deckgo/deckdeckgo/issues/384))
- add a meta attribute for the display in the feed ([#361](https://github.com/deckgo/deckdeckgo/issues/361))
- mock api ([#367](https://github.com/deckgo/deckdeckgo/issues/367))
- background opacity and missing "white" in color picker ([#348](https://github.com/deckgo/deckdeckgo/issues/348))
- randomize feed ([#374](https://github.com/deckgo/deckdeckgo/issues/374))
- don't persist "grammarly" injected data ([#376](https://github.com/deckgo/deckdeckgo/issues/376))

### Fix

- non latin characters as presentation's name / url ([#385](https://github.com/deckgo/deckdeckgo/issues/385))

<a name="1.0.0-beta.5-2"></a>

# 1.0.0-beta.5-2 (2019-09-23)

### Features

- use new color picker for code ([#353](https://github.com/deckgo/deckdeckgo/issues/353))
- improve tags spacing

### Fix

- missing "description" in the published presentation ([#351](https://github.com/deckgo/deckdeckgo/issues/351))
- trim deck name when publishing (as it is use as room name for the remote control)

<a name="1.0.0-beta.5-1"></a>

# 1.0.0-beta.5-1 (2019-09-21)

### Fix

- inline editor color and link on sticky mobile devices

<a name="1.0.0-beta.5"></a>

# 1.0.0-beta.5 (2019-09-21)

### Features

- use a custom color picker to select and apply colors which still offers the platform's picker as extra choice ([#260](https://github.com/deckgo/deckdeckgo/issues/260))
- hide fullscreen and platform's color picker on iPad ([#347](https://github.com/deckgo/deckdeckgo/issues/347))

<a name="1.0.0-beta.4"></a>

# 1.0.0-beta.4 (2019-09-20)

### Breaking Changes

- publish api v2 ([#341](https://github.com/deckgo/deckdeckgo/issues/341))

### Features

- copy slides ([#313](https://github.com/deckgo/deckdeckgo/issues/313))
- add twitter meta information to published presentations ([#332](https://github.com/deckgo/deckdeckgo/issues/332))
- add "dashboard" link in left pane menu ([#330](https://github.com/deckgo/deckdeckgo/pull/330))

### Fix

- firebase not defined on slide delete ([#329](https://github.com/deckgo/deckdeckgo/pull/329))

<a name="1.0.0-beta.3-2"></a>

# 1.0.0-beta.3-2 (2019-09-15)

### Refactoring

- remote event `youtube_pause` renamed to `pause`

<a name="1.0.0-beta.3-1"></a>

# 1.0.0-beta.3-1 (2019-09-09)

### Fix

- workaround to resize `ion-app`
- highlight lines of code in Chrome

<a name="1.0.0-beta.3"></a>

# 1.0.0-beta.3 (2019-09-08)

### Breaking changes

- use uid as storage root folder instead of username (as the username could change) ([#310](https://github.com/deckgo/deckdeckgo/issues/310) and [#319](https://github.com/deckgo/deckdeckgo/issues/319))

### Features

- on the dashboard, add new actions to delete presentations on demand ([#314](https://github.com/deckgo/deckdeckgo/issues/314) and [#322](https://github.com/deckgo/deckdeckgo/issues/322))
- delete decks, slides and storage when user delete his/her account ([#310](https://github.com/deckgo/deckdeckgo/issues/310))
- add a contributing documentation page ([#304](https://github.com/deckgo/deckdeckgo/issues/304))
- in studio and starter, align text center ([#293](https://github.com/deckgo/deckdeckgo/issues/293))
- caret color ([#285](https://github.com/deckgo/deckdeckgo/issues/285))
- improve tags style ([#324](https://github.com/deckgo/deckdeckgo/issues/324))

### Fix

- change username not resolving and clearing its value ([#298](https://github.com/deckgo/deckdeckgo/issues/298))
- reflect slide deletion to the publishing ([#243](https://github.com/deckgo/deckdeckgo/issues/243))
- weird publishing issue ([#289](https://github.com/deckgo/deckdeckgo/issues/289))
- delete header and footer on `<deckgo-slide-gif/>` template ([#307](https://github.com/deckgo/deckdeckgo/issues/307))
- word "hydrated" couldn't be used in a presetnation ([#302](https://github.com/deckgo/deckdeckgo/issues/302))
- twitter url typo ([#297](https://github.com/deckgo/deckdeckgo/issues/297))
- don't persist `deckgo-reveal-list` status ([#295](https://github.com/deckgo/deckdeckgo/pull/295))
- toggle full screen using the keyboard "Escape" key ([#287](https://github.com/deckgo/deckdeckgo/issues/287))

<a name="1.0.0-beta.2"></a>

# 1.0.0-beta.2 (2019-08-30)

### Features

- display presentation name instead of "DeckDeckGo beta" ([#248](https://github.com/deckgo/deckdeckgo/issues/248))
- sort deck with "updated_at" ([#249](https://github.com/deckgo/deckdeckgo/issues/249))
- highlight selected element ([#250](https://github.com/deckgo/deckdeckgo/issues/250))
- tagging completes on pause, not on enter ([#256](https://github.com/deckgo/deckdeckgo/issues/256))
- improve images public notice ([#261](https://github.com/deckgo/deckdeckgo/issues/261))
- fullscreen open to presentation mode ([#263](https://github.com/deckgo/deckdeckgo/issues/263))

### Fix

- inline-editor: tools appearing on continuing selection ([#280](https://github.com/deckgo/deckdeckgo/issues/280))
- slide-youtube: if I reload a presentation's youtube page, it's xmas ([#262](https://github.com/deckgo/deckdeckgo/issues/262))
- first slide saved in previous deck ([#254](https://github.com/deckgo/deckdeckgo/issues/254))
- element image converted in code ([#259](https://github.com/deckgo/deckdeckgo/issues/259))

<a name="1.0.0-beta.1"></a>

# 1.0.0-beta.1 (2019-08-30)

- to infinity and beyond 🚀
