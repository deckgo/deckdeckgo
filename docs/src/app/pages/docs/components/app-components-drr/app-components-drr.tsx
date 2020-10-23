import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-components-drr',
})
export class AppComponentsDrr {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-components-drr-drag-resize-and-rotate">Drag, Resize and Rotate</h1>
          <p>The &quot;Drag, Resize and Rotate&quot; is a Web Component to drag, resize and rotate any element.</p>
          <h2 id="app-components-drr-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-components-drag-resize-rotate-showcase">Showcase</a>
            </li>
            <li>
              <a href="#app-components-drag-resize-rotate-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-components-drag-resize-rotate-from-a-cdn">Using from a CDN</a>
                </li>
                <li>
                  <a href="#app-components-drag-resize-rotate-from-npm">Install from NPM</a>
                </li>
                <li>
                  <a href="#app-components-drag-resize-rotate-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-components-drag-resize-rotate-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-components-drag-resize-rotate-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-components-drag-resize-rotate-attributes">Attributes</a>
                </li>
                <li>
                  <a href="#app-components-drag-resize-rotate-event-listeners">Event Listeners</a>
                </li>
                <li>
                  <a href="#app-components-drag-resize-rotate-theming">Theming</a>
                </li>
                <li>
                  <a href="#app-components-drag-resize-rotate-events">Events</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-components-drag-resize-rotate-examples">Examples</a>
            </li>
          </ul>
          <h2 id="app-components-drr-showcase">Showcase</h2>
          <div style={{position: 'relative', width: '200px', height: '200px', background: '#cccccc'}}>
            <deckgo-drr style={{'--width': '20%', '--height': '10%', '--top': '5%', '--left': '10%', '--rotate': '45deg'}}>
              <div style={{background: '#FF0000'}}></div>
            </deckgo-drr>
          </div>

          <h2 id="app-components-drr-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <h3 id="app-components-drr-using-from-a-cdn">Using from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> lazy image component
            from a CDN. To do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;drag-resize-rotate@latest&#47;dist&#47;deckdeckgo-drag-resize-rotate&#47;deckdeckgo-drag-resize-rotate.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-drr-install-from-npm">Install from NPM</h3>
          <p>
            Install it in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/qrcode">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;drag-resize-rotate</code>
          </deckgo-highlight-code>
          <h3 id="app-components-drr-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-components-drr-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;drag-resize-rotate&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-components-drr-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;drag-resize-rotate&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-components-drr-usage">Usage</h2>
          <p>
            The &quot;Drag, Resize and Rotate&quot; Web Component could be integrated using the tag <code>&lt;deckgo-drr/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-drr style=&quot;--width: 10%; --height: 10%; --top: 25%; --left: 10%;&quot;&gt;{'\n'} &lt;div style=&quot;background:
              green&quot;&gt;&lt;&#47;div&gt;{'\n'}&lt;&#47;deckgo-drr&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-drr-slots">Slots</h3>
          <p>The default slot is mandatory.</p>
          <h3 id="app-components-drr-attributes">Attributes</h3>
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
            <tbody>
              <tr>
                <td>unit</td>
                <td>&#39;percentage&#39;, &#39;viewport&#39; or &#39;px&#39;</td>
                <td>&#39;percentage&#39;</td>
                <td>The component could be use with percentage, viewport (vw/vh) or pixels (px) units. All relative to the container.</td>
              </tr>
              <tr>
                <td>resize</td>
                <td>boolean</td>
                <td>true</td>
                <td>Allow or not the resize actions</td>
              </tr>
              <tr>
                <td>drag</td>
                <td>&#39;x-axis&#39;, &#39;y-axis&#39;, &#39;all&#39; or &#39;none&#39;</td>
                <td>&#39;all&#39;</td>
                <td>Allow the component to be dragged in which direction</td>
              </tr>
              <tr>
                <td>rotation</td>
                <td>boolean</td>
                <td>true</td>
                <td>Allow or not the rotation of the element</td>
              </tr>
              <tr>
                <td>text</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  To be used if your slotted element is to be defined as <code>contentEditable</code>. Useful for text edition. Note that if turns to{' '}
                  <code>true</code>, the property <code>resize</code> is going to be set to <code>false</code> automatically.
                </td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-drr-event-listeners">Event listeners</h3>
          <p>The actions are triggered through interaction with the mouse or touch.</p>
          <p>
            If for some reason you would like to block the actions and listeners, set the style <code>pointer-events</code> of the container to{' '}
            <code>none</code> and also add it a class <code>deckgo-read-only</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;div class=&quot;deckgo-read-only&quot; style=&quot;pointer-events: none&quot;&gt;{'\n'}&lt;deckgo-drr style=&quot;--width: 10%; --height: 10%;
              --top: 25%; --left: 10%;&quot;&gt;{'\n'} &lt;p&gt;Element will not be selectable nor draggable, resizable and rotatable&lt;&#47;p&gt;{'\n'}
              &lt;&#47;deckgo-drr&gt;{'\n'}&lt;&#47;did&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-components-drr-theming">Theming</h3>
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
                <td>--width</td>
                <td></td>
                <td>The default width. Will be overwritten if component is modified.</td>
              </tr>
              <tr>
                <td>--height</td>
                <td></td>
                <td>The default height. Will be overwritten if component is modified.</td>
              </tr>
              <tr>
                <td>--top</td>
                <td></td>
                <td>The default top position. Will be overwritten if component is modified.</td>
              </tr>
              <tr>
                <td>--left</td>
                <td></td>
                <td>The default left position. Will be overwritten if component is modified.</td>
              </tr>
              <tr>
                <td>--rotate</td>
                <td></td>
                <td>The default rotate angle. Will be overwritten if component is rotated.</td>
              </tr>
              <tr>
                <td>--deckgo-drr-user-select</td>
                <td>&#39;none&#39;</td>
                <td>The user selection on the host component</td>
              </tr>
              <tr>
                <td>--deckgo-drr-border</td>
                <td>1px solid #3880f7f</td>
                <td>A border around the component if selected</td>
              </tr>
              <tr>
                <td>--deckgo-drr-anchor-width</td>
                <td>16px</td>
                <td>The default width of an anchor</td>
              </tr>
              <tr>
                <td>--deckgo-drr-anchor-height</td>
                <td>16px</td>
                <td>The default height of an anchor</td>
              </tr>
              <tr>
                <td>--deckgo-drr-anchor-padding-desktop</td>
                <td>16px</td>
                <td>The default padding of an anchor on desktop</td>
              </tr>
              <tr>
                <td>--deckgo-drr-anchor-padding-mobile</td>
                <td></td>
                <td>The default padding of an anchor on touch devices</td>
              </tr>
              <tr>
                <td>--deckgo-drr-anchor-border-radius</td>
                <td>50%</td>
                <td>The default border radius of an anchor</td>
              </tr>
              <tr>
                <td>--deckgo-drr-anchor-background</td>
                <td>#3880ff</td>
                <td>The background of an anchor</td>
              </tr>
              <tr>
                <td>--deckgo-drr-anchor-border</td>
                <td></td>
                <td>The border of an anchor</td>
              </tr>
              <tr>
                <td>--deckgo-drr-rotate-anchor-width</td>
                <td>24px</td>
                <td>The rotate block anchor width</td>
              </tr>
              <tr>
                <td>--deckgo-drr-rotate-anchor-height</td>
                <td>32px</td>
                <td>The rotate block anchor height</td>
              </tr>
              <tr>
                <td>--deckgo-drr-rotate-anchor-action-width</td>
                <td>16px</td>
                <td>The rotate block anchor action width</td>
              </tr>
              <tr>
                <td>--deckgo-drr-rotate-anchor-action-height</td>
                <td>16px</td>
                <td>The rotate block anchor action height</td>
              </tr>
              <tr>
                <td>--deckgo-drr-rotate-anchor-action-border-radius</td>
                <td>50%</td>
                <td>The rotate block anchor action border radius</td>
              </tr>
              <tr>
                <td>--deckgo-drr-rotate-anchor-action-background</td>
                <td></td>
                <td>The rotate block anchor action background</td>
              </tr>
              <tr>
                <td>--deckgo-drr-rotate-anchor-action-border</td>
                <td>1px solid #3880ff</td>
                <td>The rotate block anchor action border</td>
              </tr>
              <tr>
                <td>--deckgo-drr-rotate-anchor-presentation-height</td>
                <td>calc(100% - 16px - 1px)</td>
                <td>The rotate block anchor presentation block height</td>
              </tr>
              <tr>
                <td>--deckgo-drr-rotate-anchor-presentation-border-right</td>
                <td>1px solid #3880ff</td>
                <td>The rotate block anchor presentation block border right</td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-components-drr-events">Events</h3>
          <p>
            The <code>&lt;deckgo-drr/&gt;</code> component bubbles the following events:
          </p>
          <h4 id="app-components-drr-select">Select</h4>
          <p>Emitted when the component is selected or unselected. It propagates the host component itself.</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">drrSelect(HTMLElement || undefined);</code>
          </deckgo-highlight-code>
          <h4 id="app-components-drr-change">Change</h4>
          <p>Emitted when the component is modified respectively when the user stop interacting. It propagates the host component itself.</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">drrDidChange(HTMLElement);</code>
          </deckgo-highlight-code>
          <h2 id="app-components-drr-examples">Examples</h2>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-drr style=&quot;--width: 20%; --height: 10%; --top: 5%; --left: 10%; --rotate: 45deg;&quot;&gt;{'\n'} &lt;div style=&quot;background:
              red;&quot;&gt;&lt;&#47;div&gt;{'\n'}&lt;&#47;deckgo-drr&gt;{'\n'}
              {'\n'}&lt;deckgo-drr style=&quot;--width: 10%; --height: 20%; --top: 45%; --left: 8%;&quot; drag=&quot;y-axis&quot;&gt;{'\n'} &lt;div
              style=&quot;background: yellow&quot;&gt;&lt;&#47;div&gt;{'\n'}&lt;&#47;deckgo-drr&gt;{'\n'}
              {'\n'}&lt;deckgo-drr style=&quot;--width: 10%; --height: 10%; --top: 25%; --left: 10%;&quot; resize=&quot;false&quot;&gt;{'\n'} &lt;div
              style=&quot;background: green&quot;&gt;&lt;&#47;div&gt;{'\n'}&lt;&#47;deckgo-drr&gt;{'\n'}
              {'\n'}&lt;deckgo-drr style=&quot;--width: 10%; --height: 15%; --top: 25%; --left: 15%;&quot; drag=&quot;none&quot;&gt;{'\n'} &lt;div
              style=&quot;background: greenyellow; border-radius: 50%;&quot; slot&gt;&lt;&#47;div&gt;{'\n'}&lt;&#47;deckgo-drr&gt;{'\n'}
              {'\n'}&lt;deckgo-drr unit=&quot;px&quot; style=&quot;--width: 34px; --height: 20px; --top: 40px; --left: 25px;&quot;&gt;{'\n'} &lt;img
              src=&quot;https:&#47;&#47;deckdeckgo.com&#47;assets&#47;img&#47;deckdeckgo-logo.svg&quot; style=&quot;display: none;&quot; &#47;&gt;{'\n'}
              &lt;&#47;deckgo-drr&gt;{'\n'}
              {'\n'}&lt;deckgo-drr style=&quot;--width: 34%; --height: 20%; --top: 40%; --left: 25%;&quot;&gt;{'\n'} &lt;img
              src=&quot;https:&#47;&#47;deckdeckgo.com&#47;assets&#47;img&#47;deckdeckgo-logo.svg&quot; style=&quot;display: none;&quot; &#47;&gt;{'\n'}
              &lt;&#47;deckgo-drr&gt;
            </code>
          </deckgo-highlight-code>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
