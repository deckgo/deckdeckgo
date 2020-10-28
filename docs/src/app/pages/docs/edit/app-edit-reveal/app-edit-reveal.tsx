import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-edit-reveal',
})
export class AppEditReveal {
  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-edit-reveal-reveal">Reveal</h1>
          <p>
            Make elements and text appear one line at a time in <a href="https://deckdeckgo.com">DeckDeckGo</a>.
          </p>
          <h2 id="app-edit-reveal-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-edit-reveal-introduction">Introduction</a>
            </li>
            <li>
              <a href="#app-edit-reveal-triggers">Triggers</a>
            </li>
            <li>
              <a href="#app-edit-reveal-exception">Exception</a>
            </li>
            <li>
              <a href="#app-edit-reveal-edit">Edit</a>
              <ul>
                <li>
                  <a href="#app-edit-reveal-examples">Examples</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-edit-reveal-list">List</a>
              <ul>
                <li>
                  <a href="#app-edit-reveal-examples-for-list">Examples for list</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-components-social-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-components-social-install-from-a-cdn">Using from a CDN</a>
                </li>
                <li>
                  <a href="#app-components-social-install-from-npm">Install from NPM</a>
                </li>
                <li>
                  <a href="#app-components-social-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-edit-reveal-introduction">Introduction</h2>
          <p>
            Per default all the content of each slide and component is visible. If you wish to make elements and text appear one line at a time, it&#39;s up to
            you using the following supported options.
          </p>
          <h2 id="app-edit-reveal-triggers">Triggers</h2>
          <p>
            The animation of such elements will happen when you or your user will use the navigation buttons on the keyboard or the navigation buttons in the{' '}
            <a href="https://deckdeckgo.app">remote control</a>.
          </p>
          <h2 id="app-edit-reveal-exception">Exception</h2>
          <p>
            Elements set as &quot;to be animated&quot; are going to be displayed on mobile devices, that&#39;s a design choice, but if you wish to activate the
            animation for mobile too, you could set the deck&#39;s property <code>revealOnMobile</code> to <code>true</code>.
          </p>
          <blockquote>
            <p>
              I (David here) think that it is better in terms of mobile UX. For example, if a slide would contain for example 10 elements, the users would have
              to swipe the slide 10 times before being able to read the entire content and navigate. I&#39;m open to suggestion and discussion about it, if you
              would rather like a different default behavior.
            </p>
          </blockquote>
          <h2 id="app-edit-reveal-edit">Edit</h2>
          <p>
            <a href="https://deckdeckgo.com">DeckDeckGo</a> provides a component <code>&lt;deckgo-reveal/&gt;</code> which should be used in case you would like
            to make elements appear one at a time. Simply put your element with your content inside, that&#39;s it.
          </p>
          <p>
            Good to know, the component could be use as a child of a <code>slot</code> you would pass to a slide or could also be use as <code>slot</code>{' '}
            value, as you wish.
          </p>
          <p>Nota bene, at least one element should be provided, adding only text inside the component would not work as the detection is based on elements.</p>
          <h3 id="app-edit-reveal-examples">Examples</h3>
          <p>
            The component <code>deckgo-reveal</code> use as <code>slot</code>:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'}{' '}
              &lt;deckgo-reveal slot=&quot;content&quot;&gt;{'\n'} &lt;p&gt;Hello World 🚀&lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-reveal&gt;{'\n'}{' '}
              &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <p>
            Many components <code>deckgo-reveal</code> as children:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;p
              slot=&quot;content&quot;&gt;{'\n'} &lt;deckgo-reveal&gt;&lt;span&gt;Hello World One 🚀&lt;&#47;span&gt;&lt;&#47;deckgo-reveal&gt;{'\n'}{' '}
              &lt;deckgo-reveal&gt;&lt;span&gt;Hello World Two 🚀&lt;&#47;span&gt;&lt;&#47;deckgo-reveal&gt;{'\n'} &lt;deckgo-reveal&gt;&lt;span&gt;Hello World
              Three 🚀&lt;&#47;span&gt;&lt;&#47;deckgo-reveal&gt;{'\n'} &lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <p>
            Or a component <code>deckgo-reveal</code> as child containing children:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;div
              slot=&quot;content&quot;&gt;{'\n'} &lt;deckgo-reveal&gt;{'\n'} &lt;p&gt;Hello World One 🚀&lt;&#47;p&gt;{'\n'} &lt;p&gt;Hello World Two
              🚀&lt;&#47;p&gt;{'\n'} &lt;p&gt;Hello World Three 🚀&lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-reveal&gt;{'\n'} &lt;&#47;div&gt;{'\n'}{' '}
              &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-edit-reveal-list">List</h2>
          <p>
            You could use the above component to encapsulate each <code>li</code> element of your lists, I guess that would work out, but{' '}
            <a href="https://deckdeckgo.com">DeckDeckGo</a> also provides a dedicated component <code>&lt;deckgo-reveal-list/&gt;</code> to reveal list.
          </p>
          <p>
            To use it, simply replace the opening tag of your list (<code>ul</code>, <code>ol</code> or <code>dl</code>) with it.
          </p>
          <h3 id="app-edit-reveal-attributes">Attributes</h3>
          <p>The following attributes could be applied to the element:</p>
          <table>
            <thead>
              <tr>
                <th>Property</th>
                <th>Attribute</th>
                <th>Mandatory</th>
                <th>Description</th>
                <th>Type</th>
                <th>Default</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>
                  <code>listTag</code>
                </td>
                <td>
                  <code>list-tag</code>
                </td>
                <td></td>
                <td>
                  The type of list (<code>ol</code> default, <code>ul</code> or <code>dl</code>)
                </td>
                <td>
                  <code>string</code>
                </td>
                <td>
                  <code>ol</code>
                </td>
              </tr>
            </tbody>
          </table>
          <h2 id="app-edit-reveal-theming">Theming</h2>
          <p>The following theming options are also available:</p>
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
                <td>--reveal-list-style</td>
                <td></td>
                <td>The list-style property of the list</td>
              </tr>
              <tr>
                <td>--reveal-list-style-image</td>
                <td></td>
                <td>The list-style property of the list</td>
              </tr>
              <tr>
                <td>--reveal-list-style-position</td>
                <td></td>
                <td>The list-style-position property of the list</td>
              </tr>
              <tr>
                <td>--reveal-list-margin</td>
                <td></td>
                <td>The list-margin property of the list</td>
              </tr>
              <tr>
                <td>--reveal-list-padding</td>
                <td></td>
                <td>The list-padding property of the list</td>
              </tr>
              <tr>
                <td>--reveal-list-background</td>
                <td></td>
                <td>The list-background property of the list</td>
              </tr>
              <tr>
                <td>--reveal-list-style-type</td>
                <td>
                  <code>disc</code>
                </td>
                <td>
                  The list-style-type property in case of <code>ul</code> container
                </td>
              </tr>
              <tr>
                <td>--reveal-list-style-type</td>
                <td>
                  <code>decimal</code>
                </td>
                <td>
                  The list-style-type property in case of <code>ol</code> container
                </td>
              </tr>
              <tr>
                <td>--reveal-list-style-type</td>
                <td>
                  <code>none</code>
                </td>
                <td>
                  The list-style-type property in case of <code>dl</code> container
                </td>
              </tr>
              <tr>
                <td>--reveal-opacity-not-loaded</td>
                <td>
                  <code>0</code>
                </td>
                <td>If not displayed, the component is hidden</td>
              </tr>
              <tr>
                <td>--reveal-opacity-loaded</td>
                <td>
                  <code>1</code>
                </td>
                <td>The opacity if displayed</td>
              </tr>
              <tr>
                <td>--reveal-list-opacity-not-loaded</td>
                <td>
                  <code>0</code>
                </td>
                <td>If not displayed, the component is hidden</td>
              </tr>
              <tr>
                <td>--reveal-list-opacity-loaded</td>
                <td>
                  <code>1</code>
                </td>
                <td>The opacity if displayed</td>
              </tr>
              <tr>
                <td>--reveal-transition</td>
                <td>
                  <code>opacity 0.15s linear</code>
                </td>
                <td>The animation of the component</td>
              </tr>
              <tr>
                <td>--reveal-list-transition</td>
                <td>
                  <code>opacity 0.15s linear</code>
                </td>
                <td>The animation of the component list</td>
              </tr>
              <tr>
                <td>--reveal-display</td>
                <td>
                  <code>block</code>
                </td>
                <td>The display property of the component</td>
              </tr>
              <tr>
                <td>--reveal-list-display</td>
                <td>
                  <code>opacity 0.15s linear</code>
                </td>
                <td>The display property of the component list</td>
              </tr>
            </tbody>
          </table>
          <h2 id="app-edit-reveal-examples-for-list">Examples for list</h2>
          <p>
            Likewise, the component could be used as a child of a <code>slot</code> you would pass to a slide or could also be use as <code>slot</code> value,
            as you wish.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;div
              slot=&quot;content&quot;&gt;{'\n'} &lt;deckgo-reveal-list list-tag=&quot;ul&quot;&gt;{'\n'} &lt;li&gt;Hello World One 🚀&lt;&#47;li&gt;{'\n'}{' '}
              &lt;li&gt;Hello World Two 🚀&lt;&#47;li&gt;{'\n'} &lt;li&gt;Hello World Three 🚀&lt;&#47;li&gt;{'\n'} &lt;&#47;deckgo-reveal-list&gt;{'\n'}{' '}
              &lt;&#47;div&gt;{'\n'} &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-edit-reveal-installation">Installation</h2>
          <p>This component could be added to your web application using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-edit-reveal-using-from-a-cdn">Using from a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use the <a href="https://deckdeckgo.com">DeckDeckGo</a> reveal component from
            a CDN. To do so, add the following include script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;reveal@latest&#47;dist&#47;deckdeckgo-reveal&#47;deckdeckgo-reveal.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-edit-reveal-install-from-npm">Install from NPM</h3>
          <p>
            Install it in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/reveal">npm</a> using the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;reveal</code>
          </deckgo-highlight-code>
          <h3 id="app-edit-reveal-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-edit-reveal-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;reveal&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-edit-reveal-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoElement &#125; from &#039;@deckdeckgo&#47;reveal&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoElement();
            </code>
          </deckgo-highlight-code>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
