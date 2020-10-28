import {Component, Element, h, State} from '@stencil/core';

@Component({
  tag: 'app-root',
  styleUrl: 'app-root.scss',
})
export class AppRoot {
  @Element() el: HTMLElement;

  @State()
  private loading: boolean = true;

  componentDidLoad() {
    this.loading = false;
  }

  render() {
    return [
      <ion-app class={this.loading ? 'loading' : undefined}>
        <ion-router useHash={false}>
          <ion-route url="/" component="app-home" />

          <ion-route url="/docs" component="app-introduction" />
          <ion-route url="/docs/introduction" component="app-introduction" />
          <ion-route url="/docs/installation" component="app-installation" />
          <ion-route url="/docs/running" component="app-running" />
          <ion-route url="/docs/publishing" component="app-publishing" />
          <ion-route url="/docs/remote" component="app-remote-control" />

          <ion-route url="/edit" component="app-concept" />
          <ion-route url="/edit/default" component="app-edit-default" />
          <ion-route url="/edit/lazy" component="app-edit-lazy-loading" />
          <ion-route url="/edit/theming" component="app-edit-theming" />
          <ion-route url="/edit/fonts" component="app-edit-fonts" />
          <ion-route url="/edit/reveal" component="app-edit-reveal" />
          <ion-route url="/edit/rtl" component="app-edit-rtl" />
          <ion-route url="/edit/notes" component="app-edit-notes" />

          <ion-route url="/slides" component="app-concept" />
          <ion-route url="/slides/concept" component="app-slides-concept" />
          <ion-route url="/slides/title" component="app-slide-title" />
          <ion-route url="/slides/content" component="app-slide-content" />
          <ion-route url="/slides/split" component="app-slide-split" />
          <ion-route url="/slides/gif" component="app-slide-gif" />
          <ion-route url="/slides/bigimg" component="app-slide-big-img" />
          <ion-route url="/slides/chart" component="app-slide-chart" />
          <ion-route url="/slides/youtube" component="app-slide-youtube" />
          <ion-route url="/slides/video" component="app-slide-video" />
          <ion-route url="/slides/code" component="app-slide-code" />
          <ion-route url="/slides/author" component="app-slide-author" />
          <ion-route url="/slides/qrcode" component="app-slide-qrcode" />
          <ion-route url="/slides/countdown" component="app-slide-countdown" />
          <ion-route url="/slides/poll" component="app-slide-poll" />
          <ion-route url="/slides/aspectratio" component="app-slide-aspect-ratio" />
          <ion-route url="/slides/playground" component="app-slide-playground" />

          <ion-route url="/components" component="app-concept" />
          <ion-route url="/components/charts" component="app-components-charts" />
          <ion-route url="/components/gif" component="app-components-gif" />
          <ion-route url="/components/code" component="app-components-highlight-code" />
          <ion-route url="/components/inline-editor" component="app-components-inline-editor" />
          <ion-route url="/components/qrcode" component="app-components-qrcode" />
          <ion-route url="/components/social" component="app-components-social" />
          <ion-route url="/components/youtube" component="app-components-youtube" />
          <ion-route url="/components/lazy-img" component="app-components-lazy-img" />
          <ion-route url="/components/color" component="app-components-color" />
          <ion-route url="/components/drr" component="app-components-drr" />
          <ion-route url="/components/demo" component="app-components-demo" />
          <ion-route url="/components/math" component="app-components-math" />
          <ion-route url="/components/word-cloud" component="app-components-word-cloud" />

          <ion-route url="/deck" component="app-deck-animation" />
          <ion-route url="/deck/animation" component="app-deck-animation" />
          <ion-route url="/deck/auto-slide" component="app-deck-auto-slide" />
          <ion-route url="/deck/background" component="app-deck-background" />
          <ion-route url="/deck/header-footer" component="app-deck-header-footer" />
          <ion-route url="/deck/navigation" component="app-deck-navigation" />
          <ion-route url="/deck/pager" component="app-deck-pager" />
          <ion-route url="/deck/size" component="app-deck-size" />
          <ion-route url="/deck/extra" component="app-deck-extra-features" />
          <ion-route url="/deck/events" component="app-deck-events" />
          <ion-route url="/deck/actions" component="app-deck-actions" />

          <ion-route url="/misc" component="app-misc-open-source" />
          <ion-route url="/misc/opensource" component="app-misc-open-source" />
          <ion-route url="/misc/logo" component="app-misc-logo" />
          <ion-route url="/misc/backstory" component="app-misc-backstory" />
          <ion-route url="/misc/contact" component="app-misc-contact" />
        </ion-router>

        <ion-menu side="start" type="push" swipeGesture={false} content-id="menu-content">
          <app-navigation logo={true} menuToggle={false} navigation={false}></app-navigation>
          <ion-content>
            <ion-menu-toggle autoHide={false}>
              <ion-list>
                <ion-item-group>
                  <ion-item-divider>
                    <ion-label>Introduction</ion-label>
                  </ion-item-divider>
                  <ion-item detail={false} href="/docs/introduction" routerDirection="forward">
                    <ion-label>Getting started</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/docs/installation" routerDirection="forward">
                    <ion-label>Installation</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/docs/running" routerDirection="forward">
                    <ion-label>Running</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/docs/publishing" routerDirection="forward">
                    <ion-label>Publishing</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/docs/remote" routerDirection="forward">
                    <ion-label>Remote control</ion-label>
                  </ion-item>

                  <ion-item-divider>
                    <ion-label>Edit</ion-label>
                  </ion-item-divider>
                  <ion-item detail={false} href="/edit/default" routerDirection="forward">
                    <ion-label>HTML</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/edit/lazy" routerDirection="forward">
                    <ion-label>Lazy loading</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/edit/theming" routerDirection="forward">
                    <ion-label>Theming</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/edit/fonts" routerDirection="forward">
                    <ion-label>Fonts</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/edit/reveal" routerDirection="forward">
                    <ion-label>Reveal</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/edit/rtl" routerDirection="forward">
                    <ion-label>RTL</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/edit/notes" routerDirection="forward">
                    <ion-label>Notes</ion-label>
                  </ion-item>

                  <ion-item-divider>
                    <ion-label>Templates</ion-label>
                  </ion-item-divider>
                  <ion-item detail={false} href="/slides/concept" routerDirection="forward">
                    <ion-label>Concept</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/title" routerDirection="forward">
                    <ion-label>Title</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/content" routerDirection="forward">
                    <ion-label>Content</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/split" routerDirection="forward">
                    <ion-label>Split</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/gif" routerDirection="forward">
                    <ion-label>GIF</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/bigimg" routerDirection="forward">
                    <ion-label>Big Image</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/chart" routerDirection="forward">
                    <ion-label>Chart</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/youtube" routerDirection="forward">
                    <ion-label>YouTube</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/video" routerDirection="forward">
                    <ion-label>Video</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/code" routerDirection="forward">
                    <ion-label>Code</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/author" routerDirection="forward">
                    <ion-label>Author</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/qrcode" routerDirection="forward">
                    <ion-label>QR Code</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/countdown" routerDirection="forward">
                    <ion-label>Countdown</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/poll" routerDirection="forward">
                    <ion-label>Poll</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/aspectratio" routerDirection="forward">
                    <ion-label>Aspect Ratio</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/slides/playground" routerDirection="forward">
                    <ion-label>Playground</ion-label>
                  </ion-item>

                  <ion-item-divider>
                    <ion-label>Components</ion-label>
                  </ion-item-divider>
                  <ion-item detail={false} href="/components/charts" routerDirection="forward">
                    <ion-label>Charts</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/color" routerDirection="forward">
                    <ion-label>Color Picker</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/demo" routerDirection="forward">
                    <ion-label>Demo</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/drr" routerDirection="forward">
                    <ion-label>Drag, Resize and Rotate</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/gif" routerDirection="forward">
                    <ion-label>GIF</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/code" routerDirection="forward">
                    <ion-label>Highlight Code</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/lazy-img" routerDirection="forward">
                    <ion-label>Lazy Image</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/math" routerDirection="forward">
                    <ion-label>Math</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/qrcode" routerDirection="forward">
                    <ion-label>QR Code</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/social" routerDirection="forward">
                    <ion-label>Social</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/inline-editor" routerDirection="forward">
                    <ion-label>WYSIWYG inline editor</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/word-cloud" routerDirection="forward">
                    <ion-label>Word Cloud</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/components/youtube" routerDirection="forward">
                    <ion-label>YouTube</ion-label>
                  </ion-item>

                  <ion-item-divider>
                    <ion-label>Deck</ion-label>
                  </ion-item-divider>
                  <ion-item detail={false} href="/deck/animation" routerDirection="forward">
                    <ion-label>Animation</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/deck/auto-slide" routerDirection="forward">
                    <ion-label>Auto Slide</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/deck/background" routerDirection="forward">
                    <ion-label>Background</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/deck/header-footer" routerDirection="forward">
                    <ion-label>Header &amp; Footer</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/deck/navigation" routerDirection="forward">
                    <ion-label>Navigation</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/deck/pager" routerDirection="forward">
                    <ion-label>Pager</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/deck/size" routerDirection="forward">
                    <ion-label>Size</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/deck/extra" routerDirection="forward">
                    <ion-label>Extra</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/deck/events" routerDirection="forward">
                    <ion-label>Events</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/deck/actions" routerDirection="forward">
                    <ion-label>Actions</ion-label>
                  </ion-item>

                  <ion-item-divider>
                    <ion-label>Miscellaneous</ion-label>
                  </ion-item-divider>
                  <ion-item detail={false} href="/misc/collections" routerDirection="forward">
                    <ion-label>Collections</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/misc/opensource" routerDirection="forward">
                    <ion-label>Open source</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/misc/logo" routerDirection="forward">
                    <ion-label>Logo</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/misc/backstory" routerDirection="forward">
                    <ion-label>Backstory</ion-label>
                  </ion-item>
                  <ion-item detail={false} href="/misc/contact" routerDirection="forward">
                    <ion-label>Contact</ion-label>
                  </ion-item>
                </ion-item-group>
              </ion-list>
            </ion-menu-toggle>

            <app-menu-footer class="ion-padding"></app-menu-footer>
          </ion-content>
        </ion-menu>

        <ion-nav id="menu-content" />
      </ion-app>,
    ];
  }
}
