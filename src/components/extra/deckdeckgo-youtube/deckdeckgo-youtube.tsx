import {Component, Element, Prop} from '@stencil/core';


@Component({
  tag: 'deckgo-youtube',
  styleUrl: 'deckdeckgo-youtube.scss',
  shadow: true
})
export class DeckdeckgoYoutube {

  @Element() el: HTMLElement;

  @Prop() src: string;
  @Prop() width: number;
  @Prop() height: number;

  async componentDidLoad() {
    await this.createIFrame();
  }

  private createIFrame(): Promise<void> {
    return new Promise<void>((resolve) => {
      const element: HTMLIFrameElement = document.createElement('iframe');

      const allow: Attr = document.createAttribute('allow');
      allow.value = 'accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture';

      const allowFullScreen: Attr = document.createAttribute('allowfullscreen');
      allowFullScreen.value = '';

      element.setAttributeNode(allow);
      element.setAttributeNode(allowFullScreen);

      element.src = this.src;
      element.width = '' + this.width;
      element.height = '' + this.height;
      element.frameBorder = '0';

      const div: HTMLElement = this.el.shadowRoot.querySelector('div');

      if (!div) {
        resolve();
        return;
      }

      div.appendChild(element);

      resolve();
    });
  }

  render() {
    return <div></div>
  }

}
