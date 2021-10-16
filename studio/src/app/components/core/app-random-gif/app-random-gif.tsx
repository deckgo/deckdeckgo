import {Component, ComponentInterface, h, Host, Listen, Prop, State} from '@stencil/core';

import {TenorProvider} from '../../../providers/tenor/tenor.provider';

@Component({
  tag: 'app-random-gif',
  styleUrl: 'app-random-gif.scss'
})
export class AppRandomGif implements ComponentInterface {
  @Prop()
  keyword: string;

  @State()
  private gif: TenorGif | undefined;

  @State()
  private imgLoaded: boolean = false;

  private tenorProvider: TenorProvider;

  constructor() {
    this.tenorProvider = TenorProvider.getInstance();
  }

  componentWillLoad() {
    this.initRandomGifUrl().then((gif: TenorGif | undefined) => (this.gif = gif));
  }

  @Listen('innerImgDidLoad')
  onInnerImgDidLoad() {
    this.imgLoaded = true;
  }

  private async initRandomGifUrl(): Promise<TenorGif | undefined> {
    if (!this.keyword || this.keyword === '') {
      return undefined;
    }

    const gifResponse: TenorSearchResponse | undefined = await this.tenorProvider.getRandomGif(this.keyword);

    return gifResponse?.results?.[0];
  }

  render() {
    return <Host class={`ion-margin ${this.imgLoaded ? 'imgLoaded' : ''}`}>{this.renderGif()}</Host>;
  }

  private renderGif() {
    if (this.gif?.media?.[0]?.nanogif?.url) {
      return (
        <deckgo-lazy-img
          imgSrc={this.gif.media[0].nanogif.url}
          imgAlt={this.gif.title ? this.gif.title : this.gif.media[0].nanogif.url}></deckgo-lazy-img>
      );
    }

    return undefined;
  }
}
