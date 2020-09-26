import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-code',
  styleUrl: 'app-slide-code.scss',
})
export class AppSlideCode {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  @Listen('slidesDidLoad')
  async onSlidesDidLoad($event: CustomEvent) {
    if ($event) {
      await DeckdeckgoDocsUtils.initSlideSize($event.target as HTMLElement);
    }
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-slide-code-slide-code">Slide: Code</h1>
          <p>The &quot;Code&quot; slide is a the slide to use if you would like to showcase code during your talk.</p>
          <h2 id="app-slide-code-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-code-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-code-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-code-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-code-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-code-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-code-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-code-usage-with-file-uri">Usage with file URI</a>
                </li>
                <li>
                  <a href="#app-slide-code-usage-with-slotted-element">Usage with slotted element</a>
                </li>
                <li>
                  <a href="#app-slide-code-slots">Slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-code-code-components">Code components</a>
            </li>
            <li>
              <a href="#app-slide-code-installation">Installation</a>
            </li>
            <li>
              <a href="#app-slide-code-attributes">Attributes</a>
              <ul>
                <li>
                  <a href="#app-slide-code-example-with-file-uri">Example with file URI</a>
                </li>
                <li>
                  <a href="#app-slide-code-example-with-slotted-element">Example with slotted element</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-code-theming">Theming</a>
            </li>
          </ul>
          <h2 id="app-slide-code-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-code src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/slides/code/src/components/slide/deckdeckgo-slide-code.tsx">
                <h1 slot="title">slot="title"</h1>
              </deckgo-slide-code>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-code-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-code-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-code@latest&#47;dist&#47;deckdeckgo-slide-code&#47;deckdeckgo-slide-code.esm.js&quot;&gt;&lt;&#47;script&gt;
              {'\n'}&lt;script nomodule=&quot;&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-code@latest&#47;dist&#47;deckdeckgo-slide-code&#47;deckdeckgo-slide-code.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-code-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-code">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-code</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-code-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-code-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-code&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-code-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-code&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-code-usage">Usage</h2>
          <p>
            The &quot;Code&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-code/&gt;</code>.
          </p>
          <p>You could provide a file URI to the code you want to display or provide it with a slotted element.</p>
          <h3 id="app-slide-code-usage-with-file-uri">Usage with file URI</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-code src=&quot;https:&#47;&#47;domain.com&#47;path-to-my-code.extension&quot;&gt;{'\n'} &lt;h1
              slot=&quot;title&quot;&gt;My code&lt;&#47;h1&gt;{'\n'} &lt;&#47;deckgo-slide-code&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-code-usage-with-slotted-element">Usage with slotted element</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-code language=&quot;java&quot;&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;Manual code&lt;&#47;h1&gt;
              {'\n'} &lt;code slot=&quot;code&quot;&gt;{'\n'} interface DeckDeckGoDemo &#123;{'\n'} boolean helloWorld();{'\n'} &#125;{'\n'}{' '}
              &lt;&#47;deckgo-slide-code&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-code-slots">Slots</h3>
          <p>
            The slots <code>title</code> and <code>code</code> are optional.
          </p>
          <h2 id="app-slide-code-code-components">Code components</h2>
          <p>
            The slide &quot;Code&quot; relies on the code component <code>&lt;deckgo-highlight-code/&gt;</code> which is described in the components{' '}
            <a href="https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md">documentation</a>.
          </p>
          <h2 id="app-slide-code-installation-1">Installation</h2>
          <p>
            The <a href="https://deckdeckgo.com">DeckDeckGo</a> - Hightlight code component is provided in separate extra library. If you don&#39;t use the{' '}
            <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to add the <a href="https://deckdeckgo.com">DeckDeckGo</a> code to your
            project, you will need to install and integrate it from a CDN or <a href="https://www.npmjs.com/package/@deckdeckgo/highlight-code">npm</a> as
            described in its <a href="https://docs.deckdeckgo.com/components/code#app-components-highlight-code-getting-started">installation guide</a>.
          </p>
          <h2 id="app-slide-code-attributes">Attributes</h2>
          <p>
            At least <code>src</code> or the <code>slot</code> code should be provided in order to render code in this template. It offers the same attributes
            as the <a href="https://deckdeckgo.com">DeckDeckGo</a> code Web Component, see its{' '}
            <a href="https://docs.deckdeckgo.com/components/code">documentation</a> for the details and the following other attributes:
          </p>
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
                <td>custom-background</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  If you would provide a background for the all deck and a specific one for this slide, set this option to <code>true</code>
                </td>
              </tr>
              <tr>
                <td>custom-actions</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  If you would provide actions for the all deck and a specific one for this slide, set this option to <code>true</code>
                </td>
              </tr>
            </tbody>
          </table>
          <h3 id="app-slide-code-example-with-file-uri">Example with file URI</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-code hide-anchor=&quot;false&quot;
              src=&quot;https:&#47;&#47;raw.githubusercontent.com&#47;fluster&#47;deckdeckgo&#47;master&#47;src&#47;components&#47;slides&#47;deckdeckgo-slide-code&#47;deckdeckgo-slide-code.tsx&quot;&gt;
              {'\n'} &lt;h1 slot=&quot;title&quot;&gt;Code&lt;&#47;h1&gt;{'\n'} &lt;&#47;deckgo-slide-code&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-code-example-with-slotted-element">Example with slotted element</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-code language=&quot;java&quot;&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;Manual code&lt;&#47;h1&gt;
              {'\n'} &lt;code slot=&quot;code&quot;&gt;interface NumericTest &#123;{'\n'} boolean computeTest(int n);{'\n'} &#125;{'\n'}
              {'\n'} public static void main(String args[]) &#123;{'\n'} NumericTest isEven = (n) -&gt; (n % 2) == 0;{'\n'} NumericTest isNegative = (n) -&gt;
              (n &lt; 0);{'\n'}
              {'\n'} &#47;&#47; Output: false{'\n'} System.out.println(isEven.computeTest(5));{'\n'}
              {'\n'} &#47;&#47; Output: true{'\n'} System.out.println(isNegative.computeTest(-5));{'\n'} &#125;&lt;&#47;code&gt;{'\n'}{' '}
              &lt;&#47;deckgo-slide-code&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-code-theming">Theming</h2>
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
                <td>--background</td>
                <td></td>
                <td></td>
              </tr>
              <tr>
                <td>--color</td>
                <td></td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-padding-top</td>
                <td>16px</td>
                <td>Padding top of the all slide</td>
              </tr>
              <tr>
                <td>--slide-padding-end</td>
                <td>32px</td>
                <td>Padding right of the all slide</td>
              </tr>
              <tr>
                <td>--slide-padding-bottom</td>
                <td>16px</td>
                <td>Padding bottom of the all slide</td>
              </tr>
              <tr>
                <td>--slide-padding-start</td>
                <td>32px</td>
                <td>Padding left of the all slide</td>
              </tr>
            </tbody>
          </table>
          <p>
            Furthermore, this slide component offers the exact same CSS4 variables as the <a href="https://deckdeckgo.com">DeckDeckGo</a> - Highlight code Web
            Component, see its <a href="https://docs.deckdeckgo.com/components/code">documentation</a> for the details.
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
