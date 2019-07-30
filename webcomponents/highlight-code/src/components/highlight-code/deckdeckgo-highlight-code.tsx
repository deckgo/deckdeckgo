import { Component, Prop, Watch, Element, Method, EventEmitter, Event, Listen, State, h, Host } from '@stencil/core';

import Prism from 'prismjs';

import { DeckdeckgoHighlightCodeAnchor } from '../declarations/deckdeckgo-highlight-code-anchor';

@Component({
  tag: 'deckgo-highlight-code',
  styleUrl: 'deckdeckgo-highlight-code.scss',
  shadow: true
})
export class DeckdeckgoHighlightCode {

  @Element() el: HTMLElement;

  @Event() private prismLanguageLoaded: EventEmitter<string>;
  @Event() private codeDidChange: EventEmitter<HTMLElement>;

  @Prop() src: string;

  @Prop() anchor: string = '// DeckDeckGo';
  @Prop() anchorZoom: string = '// DeckDeckGoZoom';
  @Prop() hideAnchor: boolean = true;

  @Prop({ reflectToAttr: true }) language: string = 'javascript';

  @Prop({ reflectToAttr: true }) highlightLines: string;
  @Prop({ reflectToAttr: true }) lineNumbers: boolean = false;

  @Prop() editable: boolean = false;

  @State() editing: boolean = false;

  private anchorOffsetTop: number = 0;

  async componentDidLoad() {
    const languageWasLoaded: boolean = await this.languageDidLoad();

    await this.loadLanguage();

    if (languageWasLoaded) {
      await this.fetchOrParse();
    }
  }

  @Listen('prismLanguageLoaded', { target: 'document' })
  async languageLoaded($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    if (this.language && this.language !== 'javascript' && $event.detail === this.language) {
      await this.fetchOrParse();
    }
  }

  private async fetchOrParse() {
    if (this.src) {
      await this.fetchCode();
    } else {
      await this.parseSlottedCode();
    }
  }

  private languageDidLoad(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!document || !this.language || this.language === '') {
        resolve(false);
        return;
      }

      if (this.language === 'javascript') {
        resolve(true);
        return;
      }

      const scripts = document.querySelector('[deckdeckgo-prism-loaded=\'' + this.language + '\']');
      if (scripts) {
        resolve(true);
      } else {
        resolve(false);
      }
    });
  }

  @Watch('language')
  loadLanguage(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!document || !this.language || this.language === '' || this.language === 'javascript') {
        resolve();
        return;
      }

      const scripts = document.querySelector('[deckdeckgo-prism=\'' + this.language + '\']');
      if (scripts) {
        resolve();
        return;
      }

      const script = document.createElement('script');

      script.onload = async () => {
        script.setAttribute('deckdeckgo-prism-loaded', this.language);
        this.prismLanguageLoaded.emit(this.language);
      };

      script.src = 'https://unpkg.com/prismjs@latest/components/prism-' + this.language + '.js';
      script.setAttribute('deckdeckgo-prism', this.language);
      script.defer = true;

      document.head.appendChild(script);

      resolve();
    });
  }

  @Watch('lineNumbers')
  async onLineNumbersChange() {
    await this.fetchOrParse();
  }

  @Method()
  load(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.language || this.language === '') {
        resolve();
        return;
      }

      if (this.language === 'javascript') {
        await this.fetchOrParse();
        resolve();
        return;
      }

      if (document.querySelector('[deckdeckgo-prism-loaded=\'' + this.language + '\']')) {
        await this.fetchOrParse();
      } else {
        await this.loadLanguage();
      }

      resolve();
    });
  }

  private parseSlottedCode(): Promise<void> {

    const code: HTMLElement = this.el.querySelector('[slot=\'code\']');

    if (code) {
      return this.parseCode(code.innerText ? code.innerText.trim() : code.innerText);
    } else {
      return new Promise<void>((resolve) => {
        resolve();
      });
    }
  }

  async fetchCode() {
    if (!this.src) {
      return;
    }

    let fetchedCode: string;
    try {
      const response: Response = await fetch(this.src);
      fetchedCode = await response.text();

      await this.parseCode(fetchedCode);
    } catch (e) {
      // Prism might not be able to parse the code for the selected language
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-highlight-code-container');

      if (container && fetchedCode) {
        container.children[0].innerHTML = fetchedCode;
      }
    }
  }

  private parseCode(code: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-highlight-code-container');

      if (container) {
        try {
          if (this.lineNumbers) {
            // clear the container first
            container.children[0].innerHTML = '';

            // split the code on linebreaks
            const regEx = RegExp(/\n(?!$)/g); //
            const match = code.split(regEx);
            match.forEach(m => {
              let div: HTMLElement = document.createElement('div');
              div.classList.add('deckgo-highlight-code-line-number');

              let highlight: string = Prism.highlight(m, Prism.languages[this.language], this.language);
              div.innerHTML = highlight;
              container.children[0].appendChild(div);
            });
          }else{
            container.children[0].innerHTML = Prism.highlight(code, Prism.languages[this.language], this.language);
          }

          await this.addAnchors();

          setTimeout(async () => {
            await this.addHighlight();
          }, 100);

          resolve();
        } catch (err) {
          reject(err);
        }
      }
    });
  }

  private addAnchors(): Promise<void> {
    return new Promise<void>((resolve) => {
      const elements: NodeListOf<HTMLElement> = this.el.shadowRoot.querySelectorAll('span.comment');

      if (elements) {
        const elementsArray: HTMLElement[] = Array.from(elements);

        const anchors: HTMLElement[] = elementsArray.filter((element: HTMLElement) => {
          return this.hasLineAnchor(element.innerHTML);
        });

        if (anchors) {
          anchors.forEach((anchor: HTMLElement) => {
            anchor.classList.add('deckgo-highlight-code-anchor');

            if (this.hideAnchor) {
              anchor.classList.add('deckgo-highlight-code-anchor-hidden');
            }
          });
        }
      }

      resolve();
    });
  }

  private hasLineAnchor(line: string): boolean {
    return line && this.anchor &&
      line.indexOf('@Prop') === -1 &&
      line.split(' ').join('').indexOf(this.anchor.split(' ').join('')) > -1;
  }

  private addHighlight(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.highlightLines && this.highlightLines.length > 0) {

        const rows: number[] = await this.findRowsToHighlight();
        if (rows && rows.length > 0) {
          const containerCode: HTMLElement = this.el.shadowRoot.querySelector('code');

          if (containerCode && containerCode.hasChildNodes()) {
            const elements: HTMLElement[] = Array.prototype.slice.call(containerCode.childNodes);

            let rowIndex: number = -1;
            let lastOffsetTop: number = -1;
            let offsetHeight: number = -1;

            elements.forEach((element: HTMLElement) => {

              let editElement: HTMLElement;

              // We need to convert text entries to an element in order to be able to style it
              if (element.nodeName === '#text') {
                const span = document.createElement('span');

                if (element.previousElementSibling) {
                  element.previousElementSibling.insertAdjacentElement('afterend', span);
                } else {
                  element.parentNode.prepend(span);
                }

                span.appendChild(element);

                editElement = span;
              } else {
                editElement = element;
              }

              // We try to find the row index with the offset of the element
              rowIndex = editElement.offsetTop > lastOffsetTop ? (rowIndex + 1) : rowIndex;
              lastOffsetTop = editElement.offsetTop;

              // For some reason, some converted text element are displayed on two lines, that's why we should consider the 2nd one as index
              offsetHeight = offsetHeight === -1 || offsetHeight > editElement.offsetHeight ? editElement.offsetHeight : offsetHeight;

              const rowsIndexToCompare: number = editElement.offsetHeight > offsetHeight ? (rowIndex + 1) : rowIndex;

              if (rows.indexOf(rowsIndexToCompare) > -1) {
                editElement.classList.add('deckgo-highlight-code-line');
              }
            })
          }

        }
      }

      resolve();
    });
  }

  private findRowsToHighlight(): Promise<number[]> {
    return new Promise<number[]>((resolve) => {
      let results: number[] = [];

      const rows: string[] = this.highlightLines.split(' ');

      if (rows && rows.length > 0) {
        rows.forEach((row: string) => {
          const index: string[] = row.split(',');

          if (index && index.length >= 1) {
            const start: number = parseInt(index[0], 0);
            const end: number = parseInt(index[1], 0);

            for (let i = start; i <= end; i++) {
              results.push(i);
            }
          }
        });
      }

      resolve(results);
    });
  }

  @Method()
  findNextAnchor(enter: boolean): Promise<DeckdeckgoHighlightCodeAnchor> {
    return new Promise<DeckdeckgoHighlightCodeAnchor>(async (resolve) => {
      const elements: NodeListOf<HTMLElement> = this.el.shadowRoot.querySelectorAll('span.deckgo-highlight-code-anchor');

      if (elements) {
        const elementsArray: HTMLElement[] = enter ? Array.from(elements) : Array.from(elements).reverse();

        const anchor: HTMLElement = elementsArray.find((element: HTMLElement) => {
          return enter ? element.offsetTop > this.anchorOffsetTop : element.offsetTop < this.anchorOffsetTop;
        });

        if (anchor) {
          this.anchorOffsetTop = anchor.offsetTop;

          resolve({
            offsetTop: anchor.offsetTop,
            hasLineZoom: this.hasLineZoom(anchor.textContent)
          });
        } else if (!enter) {
          const elementCode: HTMLElement = this.el.shadowRoot.querySelector('code');

          if (elementCode && elementCode.firstElementChild) {
            this.anchorOffsetTop = 0;

            resolve({
              offsetTop: 0,
              hasLineZoom: false
            });
          } else {
            resolve(null);
          }
        } else {
          resolve(null);
        }
      } else {
        resolve(null);
      }
    });
  }

  @Method()
  zoomCode(zoom: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-highlight-code-container');

      if (container) {
        container.style.setProperty('--deckgo-highlight-code-zoom', zoom ? '1.3' : '1');
      }

      resolve();
    });
  }

  private hasLineZoom(line: string): boolean {
    return line && this.anchorZoom &&
      line.indexOf('@Prop') === -1 &&
      line.split(' ').join('').indexOf(this.anchorZoom.split(' ').join('')) > -1;
  }

  private edit(): Promise<void> {
    return new Promise<void>((resolve) => {

      if (!this.editable) {

        resolve();
        return;
      }

      this.editing = true;

      const slottedCode: HTMLElement = this.el.querySelector('[slot=\'code\']');

      if (slottedCode) {
        setTimeout(() => {
          slottedCode.setAttribute('contentEditable', 'true');
          slottedCode.addEventListener('blur', this.applyCode, { once: true });
          slottedCode.addEventListener('keydown', this.catchNewLine);

          slottedCode.focus();
        }, 100);
      }

      resolve();
    });
  }

  private catchNewLine = async ($event: KeyboardEvent) => {
    if ($event && $event.key === 'Enter') {
      $event.preventDefault();

      const selection: Selection = await this.getSelection();
      if (selection && selection.focusNode && selection.focusNode.textContent && selection.focusOffset > 0) {
        const charCode: number = selection.focusNode.textContent.charCodeAt(window.getSelection().focusOffset);

        document.execCommand('insertHTML', false, charCode === 10 || charCode === 13 ? '\n' : '\n\n');
      } else {
        document.execCommand('insertHTML', false, '\n\n');
      }
    }
  };

  private getSelection(): Promise<Selection> {
    return new Promise<Selection>((resolve) => {
      let selectedSelection: Selection = null;

      if (window && window.getSelection) {
        selectedSelection = window.getSelection();
      } else if (document && document.getSelection) {
        selectedSelection = document.getSelection();
      } else if (document && (document as any).selection) {
        selectedSelection = (document as any).selection.createRange().text;
      }

      resolve(selectedSelection);
    });
  }

  private applyCode = async () => {
    await this.stopEditing();

    await this.parseSlottedCode();
  };

  private stopEditing(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.editing = false;

      const slottedCode: HTMLElement = this.el.querySelector('[slot=\'code\']');

      if (slottedCode) {
        slottedCode.removeAttribute('contentEditable');

        if (slottedCode.innerHTML) {
          slottedCode.innerHTML = slottedCode.innerHTML.trim();
        }

        this.codeDidChange.emit(this.el);
      }

      resolve();
    });
  }

  render() {
    return (
      <Host class={{ 'deckgo-highlight-code-edit': this.editing }}>
        <div class="deckgo-highlight-code-container"
          onMouseDown={() => this.edit()}
          onTouchStart={() => this.edit()}>
          <code></code>
          <slot name="code"></slot>
        </div>
      </Host>
    );
  }

}
