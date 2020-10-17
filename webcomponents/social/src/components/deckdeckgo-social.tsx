import {Component, Method, Prop, State, Watch, h, Host} from '@stencil/core';

import {DeckdeckgoComponent} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-social',
  styleUrl: 'deckdeckgo-social.scss',
  shadow: true,
})
export class DeckdeckgoSocial implements DeckdeckgoComponent {
  @Prop({reflect: true}) twitter: string;
  @Prop({reflect: true}) linkedin: string;
  @Prop({reflect: true}) medium: string;
  @Prop({reflect: true}) dev: string;
  @Prop({reflect: true}) github: string;
  @Prop({reflect: true}) fullUrl: string;

  @State()
  private url: string;

  @State()
  private ariaLabel: string;

  async componentWillLoad() {
    const promises: Promise<void>[] = [
      this.concatTwitterUrl(),
      this.concatLinkedinUrl(),
      this.concatMediumUrl(),
      this.concatDevUrl(),
      this.concatGithubUrl(),
      this.concatFullUrl(),
    ];

    await Promise.all(promises);
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return Promise.resolve();
  }

  @Watch('twitter')
  async concatTwitterUrl() {
    if (!this.twitter) {
      return;
    }

    this.url = 'https://twitter.com/' + this.twitter;
    this.ariaLabel = `twitter/${this.twitter}`;
  }

  @Watch('linkedin')
  async concatLinkedinUrl() {
    if (!this.linkedin) {
      return;
    }

    this.url = 'https://www.linkedin.com/in/' + this.linkedin;
    this.ariaLabel = `linkedin/${this.linkedin}`;
  }

  @Watch('medium')
  async concatMediumUrl() {
    if (!this.medium) {
      return;
    }

    this.url = `https://${this.medium}.medium.com`;
    this.ariaLabel = `medium/${this.medium}`;
  }

  @Watch('dev')
  async concatDevUrl() {
    if (!this.dev) {
      return;
    }

    this.url = 'https://dev.to/' + this.dev;
    this.ariaLabel = `thepracticaldev/${this.dev}`;
  }

  @Watch('github')
  async concatGithubUrl() {
    if (!this.github) {
      return;
    }

    this.url = 'https://github.com/' + this.github;
    this.ariaLabel = `github/${this.github}`;
  }

  @Watch('fullUrl')
  async concatFullUrl() {
    if (!this.fullUrl) {
      return;
    }

    this.url = this.fullUrl;

    if (this.fullUrl) {
      this.ariaLabel = this.fullUrl.replace(/http:\/\/|https:\/\/|www\./g, '');
    } else {
      this.ariaLabel = this.fullUrl;
    }
  }

  render() {
    return (
      <Host aria-label={this.ariaLabel}>
        <a href={this.url}>
          <slot name="icon"></slot>
          <slot>
            {this.twitter ? <span>{`${this.twitter}`}</span> : undefined}
            {this.linkedin ? <span>{`${this.linkedin}`}</span> : undefined}
            {this.medium ? <span>{`${this.medium}`}</span> : undefined}
            {this.dev ? <span>{`${this.dev}`}</span> : undefined}
            {this.github ? <span>{`${this.github}`}</span> : undefined}
            {this.fullUrl ? <span>{`${this.ariaLabel}`}</span> : undefined}
          </slot>
        </a>
      </Host>
    );
  }
}
