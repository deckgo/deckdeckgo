import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-components-lazy-img'
})
export class AppComponentsLazyImg {

  @Element() el: HTMLElement;

  private menuService: MenuService;

  constructor() {
    this.menuService = MenuService.getInstance();
  }

  async componentWillLoad() {
    this.menuService.enable();
  }

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main><h1 id="app-components-lazy-img-lazy-image">Lazy Image</h1>
<p>The &quot;Lazy Image&quot; component is a dead simple component to lazy load images.</p>
<p>It leverages the IntersectionObserver API to lazy load images and also allows you to trigger &quot;manually&quot; their loading.</p>
<p>An <code>&lt;img/&gt;</code> tag is per default use to display the image but optionally it could parse <code>SVG</code> too.</p>
<h2 id="app-components-lazy-img-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-components-lazy-img-usage">Usage</a><ul>
<li><a href="#app-components-lazy-img-slots">Slots</a></li>
<li><a href="#app-components-lazy-img-attributes">Attributes</a></li>
<li><a href="#app-components-lazy-img-theming">Theming</a></li>
<li><a href="#app-components-lazy-img-methods">Methods</a></li>
</ul>
</li>
<li><a href="(#app-components-lazy-img-fallback">Fallback</a></li>
<li><a href="#app-components-lazy-img-trying-it-out">Trying it out</a></li>
</ul>
<h2 id="app-components-lazy-img-usage">Usage</h2>
<p>The &quot;Lazy Image&quot; Web Component could be integrated using the tag <code>&lt;deckgo-lazy-img/&gt;</code>.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-lazy-img img-src=&quot;&#47;assets&#47;twitter.svg&quot;&gt;{'\n'}&lt;&#47;deckgo-lazy-img&gt;</code>
    </deckgo-highlight-code><h3 id="app-components-lazy-img-slots">Slots</h3>
<p>No slots are available for this component.</p>
<h3 id="app-components-lazy-img-attributes">Attributes</h3>
<p>This component offers the following options which could be set using attributes:</p>
<table>
<thead>
<tr>
<th>Attribute</th>
<th>Type</th>
<th>Default</th>
<th>Description</th>
</tr>
</thead>
<tbody><tr>
<td>img-src</td>
<td>string</td>
<td></td>
<td>The image source (= URI) to lazy load</td>
</tr>
<tr>
<td>img-src-set</td>
<td>string</td>
<td></td>
<td>The attribute &quot;srcset&quot; (= multiple URI) to lazy load in case you would like to provide multiple images for responsiveness</td>
</tr>
<tr>
<td>img-error-src</td>
<td>string</td>
<td></td>
<td>An optional image which could be displayed in case the main image would not be resolved</td>
</tr>
<tr>
<td>img-sizes</td>
<td>string</td>
<td></td>
<td>The set of media conditions to indicates what image size would be best to choose</td>
</tr>
<tr>
<td>img-alt</td>
<td>string</td>
<td></td>
<td>The image alternate text</td>
</tr>
<tr>
<td>svg-src</td>
<td>string</td>
<td></td>
<td>The SVG image source (= URI) to lazy load and to parse (no <code>&lt;img/&gt;</code> tag will be use to render the svg)</td>
</tr>
<tr>
<td>aria-label</td>
<td>string</td>
<td></td>
<td>If you are using the above SVG option, provide the accessibility information using this attribute</td>
</tr>
<tr>
<td>observer-root-margin</td>
<td>string</td>
<td>100px 0px</td>
<td>A string which specifies a set of offsets to add to the root&#39;s bounding_box when calculating intersections, effectively shrinking or growing the root for calculation purposes. <a href="https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver/IntersectionObserver">More info.</a></td>
</tr>
<tr>
<td>observer-threshold</td>
<td>number or number[]</td>
<td></td>
<td>Either a single number or an array of numbers between 0.0 and 1.0, specifying a ratio of intersection area to total bounding box area for the observed target. <a href="https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver/IntersectionObserver">More info.</a></td>
</tr>
</tbody></table>
<h3 id="app-components-lazy-img-theming">Theming</h3>
<p>The following theming options will affect this component if set on its host or parent.</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
<td>--deckgo-lazy-img-max-height</td>
<td></td>
<td>Image max height</td>
</tr>
<tr>
<td>--deckgo-lazy-img-max-width</td>
<td>100%</td>
<td>Image max width</td>
</tr>
<tr>
<td>--deckgo-lazy-img-min-height</td>
<td></td>
<td>Image min height</td>
</tr>
<tr>
<td>--deckgo-lazy-img-min-width</td>
<td></td>
<td>Image min width</td>
</tr>
<tr>
<td>--deckgo-lazy-img-pointer-events</td>
<td>none</td>
<td>Image pointer events</td>
</tr>
<tr>
<td>--deckgo-lazy-img-height</td>
<td></td>
<td>Image height</td>
</tr>
<tr>
<td>--deckgo-lazy-img-width</td>
<td></td>
<td>Image width</td>
</tr>
<tr>
<td>--deckgo-lazy-img-float</td>
<td></td>
<td>Image float</td>
</tr>
<tr>
<td>--deckgo-lazy-img-padding</td>
<td></td>
<td>Image padding</td>
</tr>
<tr>
<td>--deckgo-lazy-img-vertical-align</td>
<td></td>
<td>Image vertical alignment</td>
</tr>
<tr>
<td>--deckgo-lazy-img-display</td>
<td></td>
<td>The display property of the image</td>
</tr>
<tr>
<td>--deckgo-lazy-img-border-radius</td>
<td></td>
<td>In case you would like to specify a border radius for the image</td>
</tr>
<tr>
<td>--deckgo-lazy-img-object-fit</td>
<td></td>
<td>The property object-fit of the image</td>
</tr>
</tbody></table>
<h3 id="app-components-lazy-img-methods">Methods</h3>
<p>This component also export an async method <code>lazyLoad()</code> in case you would like to trigger &quot;manually&quot; the loading of the image.</p>
<deckgo-highlight-code language="javascript">
      <code slot="code">const element = document.querySelector(&#039;deckgo-lazy-img&#039;);{'\n'}await element.lazyLoad();</code>
    </deckgo-highlight-code><h3 id="app-components-lazy-img-fallback">Fallback</h3>
<p>In case the browsers would not support the Intersection Observer, images are going to be loaded without any delay when the component load respectively if the browser does not implement the Intersection Observer API images are displayed and not lazy loaded. </p>
<h3 id="app-components-lazy-img-trying-it-out">Trying it out</h3>
<p>This component lazy load images when these are not presented in the viewport. If you would use this component in a simple test containing only a couple of images, respectively no content or no real use case where the images are effectively offscreen, assign a default height to components in order to ensure that some are effectively placed outside of the window <a href="https://github.com/deckgo/deckdeckgo/issues/128#issuecomment-493979841">[#128]</a>.</p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
