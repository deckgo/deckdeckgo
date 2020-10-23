import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-color',
})
export class AppComponentsColor {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-components-color-color-picker">Color Picker</h1>
          <p>The &quot;Color Picker&quot; component is a simple component to, guess what, allow your users to &quot;pick colors&quot; 😉</p>
          <p>
            It is fully configurable in terms of colors, you could define the set of colors you rather like to offer and it also implements a &quot;more&quot;
            action which, if clicked, will open the platform standard color picker.
          </p>
          <h2 id="app-components-color-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#color-picker">Color Picker</a>
              <ul>
                <li>
                  <a href="#table-of-contents">Table of contents</a>
                </li>
                <li>
                  <a href="#showcase">Showcase</a>
                </li>
                <li>
                  <a href="#installation">Installation</a>
                  <ul>
                    <li>
                      <a href="#using-from-a-cdn">Using from a CDN</a>
                    </li>
                    <li>
                      <a href="#install-from-npm">Install from NPM</a>
                    </li>
                    <li>
                      <a href="#framework-integration">Framework integration</a>
                      <ul>
                        <li>
                          <a href="#import">Import</a>
                        </li>
                        <li>
                          <a href="#loader">Loader</a>
                        </li>
                      </ul>
                    </li>
                  </ul>
                </li>
                <li>
                  <a href="#usage">Usage</a>
                  <ul>
                    <li>
                      <a href="#slots">Slots</a>
                    </li>
                    <li>
                      <a href="#attributes">Attributes</a>
                      <ul>
                        <li>
                          <a href="#palette">Palette</a>
                        </li>
                      </ul>
                    </li>
                    <li>
                      <a href="#theming">Theming</a>
                    </li>
                    <li>
                      <a href="#events">Events</a>
                    </li>
                  </ul>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-components-color-showcase">Showcase</h2>
          <div>
            <deckgo-color style={{width: '240px'}}>
              <span slot="more">...</span>
            </deckgo-color>
          </div>

          <h2 id="app-components-color-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <h3 id="app-components-color-using-from-a-cdn">Using from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> lazy image component
            from a CDN. To do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;color@latest&#47;dist&#47;deckdeckgo-color&#47;deckdeckgo-color.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-color-install-from-npm">Install from NPM</h3>
          <p>
            Install it in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/qrcode">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;color</code>
          </deckgo-highlight-code>
          <h3 id="app-components-color-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-components-color-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;color&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-color-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;color&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-components-color-usage">Usage</h2>
          <p>
            The &quot;Color Picker&quot; Web Component could be integrated using the tag <code>&lt;deckgo-color/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-color&gt;{'\n'} &lt;span slot=&quot;more&quot;&gt;...&lt;&#47;span&gt;{'\n'}&lt;&#47;deckgo-color&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-color-slots">Slots</h3>
          <p>
            The slot <code>more</code> is optional, moreover, the &quot;more&quot; action itself could be turned off.
          </p>
          <p>
            The slot <code>custom-label</code> is optional and can be used to display another label than <code>Custom</code> when user selects a custom color.
          </p>
          <h3 id="app-components-color-attributes">Attributes</h3>
          <p>This component offers the following options which could be set using attributes:</p>
          <table>
            <thead>
              <tr>
                <th>Property</th>
                <th>Attribute</th>
                <th>Description</th>
                <th>Type</th>
                <th>Default</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>
                  <code>colorHex</code>
                </td>
                <td>
                  <code>color-hex</code>
                </td>
                <td>The current selected color provided as hexadecimal value.</td>
                <td>
                  <code>string</code>
                </td>
                <td>
                  <code>undefined</code>
                </td>
              </tr>
              <tr>
                <td>
                  <code>colorRgb</code>
                </td>
                <td>
                  <code>color-rgb</code>
                </td>
                <td>The current selected color provided as a rgb value.</td>
                <td>
                  <code>string</code>
                </td>
                <td>
                  <code>undefined</code>
                </td>
              </tr>
              <tr>
                <td>
                  <code>more</code>
                </td>
                <td>
                  <code>more</code>
                </td>
                <td>In case you would not like to offer the &quot;more&quot; options.</td>
                <td>
                  <code>boolean</code>
                </td>
                <td>
                  <code>true</code>
                </td>
              </tr>
              <tr>
                <td>
                  <code>moreAlt</code>
                </td>
                <td>
                  <code>more-alt</code>
                </td>
                <td>An accessibility label for the &quot;more action.</td>
                <td>
                  <code>string</code>
                </td>
                <td>
                  <code>&#39;More&#39;</code>
                </td>
              </tr>
              <tr>
                <td>
                  <code>palette</code>
                </td>
                <td></td>
                <td>The palette of color (see here under).</td>
                <td>
                  <code>DeckdeckgoPalette[]</code>
                </td>
                <td>
                  <code>DEFAULT_PALETTE</code>
                </td>
              </tr>
              <tr>
                <td>
                  <code>label</code>
                </td>
                <td>
                  <code>label</code>
                </td>
                <td>Display a label for the description of the selected color</td>
                <td>
                  <code>boolean</code>
                </td>
                <td>
                  <code>true</code>
                </td>
              </tr>
            </tbody>
          </table>
          <h4 id="app-components-color-palette">Palette</h4>
          <p>
            The <code>palette</code> attribute is a complex object and therefore could only be set using Javascript.
          </p>
          <p>It is defined as the following:</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              export interface DeckdeckgoPaletteColor &#123;{'\n'} hex: string;{'\n'} rgb?: string;{'\n'}&#125;{'\n'}
              {'\n'}export interface DeckdeckgoPalette &#123;{'\n'} color: DeckdeckgoPaletteColor;{'\n'} alt?: string;{'\n'}&#125;
            </code>
          </deckgo-highlight-code>
          <p>
            The key value is the color provided as <code>hex</code> value. The <code>rgb</code> value is use for presentation purpose, for the hover action and
            the highlight of the selected color. If you wish to highlight a selected color, you could either provide <code>color-hex</code> or{' '}
            <code>color-rgb</code>.
          </p>
          <p>The default palette is the following:</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              export const DEFAULT_PALETTE: DeckdeckgoPalette[] = [{'\n'} &#123;{'\n'} color: &#123;{'\n'} hex: &#039;#8ED1FC&#039;,{'\n'} rgb:
              &#039;142,209,252&#039;,{'\n'} &#125;,{'\n'} alt: &#039;Light blue&#039;,{'\n'} &#125;,{'\n'} &#123;{'\n'} color: &#123;{'\n'} hex:
              &#039;#0693E3&#039;,{'\n'} rgb: &#039;6,147,227&#039;,{'\n'} &#125;,{'\n'} alt: &#039;Blue&#039;,{'\n'} &#125;,{'\n'} &#123;{'\n'} color: &#123;
              {'\n'} hex: &#039;#7BDCB5&#039;,{'\n'} rgb: &#039;123,220,181&#039;,{'\n'} &#125;,{'\n'} alt: &#039;Light green&#039;,{'\n'} &#125;,{'\n'} &#123;
              {'\n'} color: &#123;{'\n'} hex: &#039;#00D084&#039;,{'\n'} rgb: &#039;0,208,132&#039;,{'\n'} &#125;,{'\n'} alt: &#039;Green&#039;,{'\n'} &#125;,
              {'\n'} &#123;{'\n'} color: &#123;{'\n'} hex: &#039;#FCB900&#039;,{'\n'} rgb: &#039;252,185,0&#039;,{'\n'} &#125;,{'\n'} alt: &#039;Yellow&#039;,
              {'\n'} &#125;,{'\n'} &#123;{'\n'} color: &#123;{'\n'} hex: &#039;#FF6900&#039;,{'\n'} rgb: &#039;255,105,0&#039;,{'\n'} &#125;,{'\n'} alt:
              &#039;Orange&#039;,{'\n'} &#125;,{'\n'} &#123;{'\n'} color: &#123;{'\n'} hex: &#039;#F78DA7&#039;,{'\n'} rgb: &#039;247,141,167&#039;,{'\n'}{' '}
              &#125;,{'\n'} alt: &#039;Pink&#039;,{'\n'} &#125;,{'\n'} &#123;{'\n'} color: &#123;{'\n'} hex: &#039;#EB144C&#039;,{'\n'} rgb:
              &#039;235,20,76&#039;,{'\n'} &#125;,{'\n'} alt: &#039;Red&#039;,{'\n'} &#125;,{'\n'} &#123;{'\n'} color: &#123;{'\n'} hex: &#039;#ffffff&#039;,
              {'\n'} rgb: &#039;255,255,255&#039;,{'\n'} &#125;,{'\n'} alt: &#039;White&#039;,{'\n'} display: &#123;{'\n'} borderColor: &#039;#ddd&#039;,{'\n'}{' '}
              boxShadowColor: &#039;221,221,221&#039;,{'\n'} &#125;,{'\n'} &#125;,{'\n'} &#123;{'\n'} color: &#123;{'\n'} hex: &#039;#ABB8C3&#039;,{'\n'} rgb:
              &#039;171,184,195&#039;,{'\n'} &#125;,{'\n'} alt: &#039;Grey&#039;,{'\n'} &#125;,{'\n'} &#123;{'\n'} color: &#123;{'\n'} hex: &#039;#000000&#039;,
              {'\n'} rgb: &#039;0,0,0&#039;,{'\n'} &#125;,{'\n'} alt: &#039;Black&#039;,{'\n'} &#125;,{'\n'}];
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-color-theming">Theming</h3>
          <p>The following theming options will affect this component if set on its host or parent.</p>
          <table>
            <thead>
              <tr>
                <th>CSS4 variable</th>
                <th>Default</th>
                <th>Note</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>--deckgo-button-width</td>
                <td>28px</td>
                <td>The width of a button to select a color and the more button</td>
              </tr>
              <tr>
                <td>--deckgo-button-height</td>
                <td>28px</td>
                <td>The height of a button to select a color and the more button</td>
              </tr>
              <tr>
                <td>--deckgo-button-margin</td>
                <td>4px</td>
                <td>The margin of a button to select a color and the more button</td>
              </tr>
              <tr>
                <td>--deckgo-button-outline</td>
                <td>none</td>
                <td>The outline of a button to select a color and the more button</td>
              </tr>
              <tr>
                <td>--deckgo-button-border</td>
                <td>none</td>
                <td>The border of a button to select a color and the more button</td>
              </tr>
              <tr>
                <td>--deckgo-button-border-radius</td>
                <td>50%</td>
                <td>The border radius of a button to select a color</td>
              </tr>
              <tr>
                <td>--deckgo-button-more-border-radius</td>
                <td>50%</td>
                <td>The border radius of the more button</td>
              </tr>
              <tr>
                <td>--deckgo-button-more-border</td>
                <td>none</td>
                <td>The border of the more button</td>
              </tr>
              <tr>
                <td>--deckgo-button-more-outline</td>
                <td>none</td>
                <td>The outline of the more button</td>
              </tr>
              <tr>
                <td>--deckgo-flex-wrap</td>
                <td>wrap</td>
                <td>Wrap properties of the buttons&#39; container</td>
              </tr>
              <tr>
                <td>--deckgo-overflow</td>
                <td>visible</td>
                <td>Overflow property of the buttons&#39;s container</td>
              </tr>
              <tr>
                <td>--deckgo-button-more-background</td>
                <td>transparent</td>
                <td>The background of the more button</td>
              </tr>
              <tr>
                <td>--deckgo-color-label-margin</td>
                <td>0</td>
                <td>Margin of the color description</td>
              </tr>
              <tr>
                <td>--deckgo-color-label-padding</td>
                <td>12px 8px</td>
                <td>Padding of the color description</td>
              </tr>
              <tr>
                <td>--deckgo-color-label-font-size</td>
                <td>12px</td>
                <td>Font size of the color description</td>
              </tr>
              <tr>
                <td>--deckgo-color-label-min-height</td>
                <td>15px</td>
                <td>Minimal height of the color description</td>
              </tr>
              <tr>
                <td>--deckgo-color-label-text-align</td>
                <td>center</td>
                <td>Text alignment of the color description</td>
              </tr>
              <tr>
                <td>--deckgo-color-label-color-font-weight</td>
                <td>300</td>
                <td>Font weight of the hexadecimal value of the color description</td>
              </tr>
            </tbody>
          </table>
          <table>
            <thead>
              <tr>
                <th>Shadow-DOM part</th>
                <th>Note</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>container</td>
                <td>Allows to style the container the color buttons are in</td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-color-events">Events</h3>
          <p>To listen to the selected color you have to subscribe to the following event:</p>
          <table>
            <thead>
              <tr>
                <th>Event</th>
                <th>Description</th>
                <th>Type</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>
                  <code>colorChange</code>
                </td>
                <td>Emit the selected color.</td>
                <td>
                  <code>CustomEvent&lt;DeckdeckgoPaletteColor&gt;</code>
                </td>
              </tr>
            </tbody>
          </table>
          <p>
            In case the platform color picker would be use by the user, the change will be triggered multiple times, as long as the user change its value in the
            platform picker.
          </p>
          <p>
            For the definition of the type of the event, see above description of <code>DeckdeckgoPaletteColor</code>.
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
