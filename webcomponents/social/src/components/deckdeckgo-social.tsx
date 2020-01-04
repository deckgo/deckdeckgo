import {Component, Method, Prop, State, Watch, h, Host} from '@stencil/core';

@Component({
  tag: 'deckgo-social',
  styleUrl: 'deckdeckgo-social.scss',
  shadow: true
})
export class DeckdeckgoSocial {

  @State() url: string;

  @Prop({reflect: true}) twitter: string;
  @Prop({reflect: true}) linkedin: string;
  @Prop({reflect: true}) medium: string;
  @Prop({reflect: true}) dev: string;
  @Prop({reflect: true}) github: string;
  @Prop({reflect: true}) fullUrl: string;

  componentWillLoad() {
    this.concatTwitterUrl();
    this.concatLinkedinUrl();
    this.concatMediumUrl();
    this.concatDevUrl();
    this.concatGithubUrl();
    this.concatFullUrl();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return new Promise<void>((resolve) => {
      resolve();
    });
  }

  @Watch('twitter')
  concatTwitterUrl() {
    if (!this.twitter) {
      return;
    }

    this.url = 'https://twitter.com/' + this.twitter;
  }

  @Watch('linkedin')
  concatLinkedinUrl() {
    if (!this.linkedin) {
      return;
    }

    this.url = 'https://www.linkedin.com/in/' + this.linkedin;
  }

  @Watch('medium')
  concatMediumUrl() {
    if (!this.medium) {
      return;
    }

    this.url = 'https://medium.com/@' + this.medium;
  }

  @Watch('dev')
  concatDevUrl() {
    if (!this.dev) {
      return;
    }

    this.url = 'https://dev.to/' + this.dev;
  }

  @Watch('github')
  concatGithubUrl() {
    if (!this.github) {
      return;
    }

    this.url = 'https://github.com/' + this.github;
  }

  @Watch('fullUrl')
  concatFullUrl() {
    if (!this.fullUrl) {
      return;
    }

    this.url = this.fullUrl;
  }

  private ariaLabel() {
    if (this.twitter) {
      return `twitter/${this.twitter}`
    } else if (this.linkedin) {
      return `linkedin/${this.linkedin}`
    } else if (this.medium) {
      return `medium/${this.medium}`
    } else if (this.dev) {
      return `thepracticaldev/${this.dev}`
    } else if (this.github) {
      return `github/${this.github}`
    } else {
      return this.fullUrl;
    }
  }

  render() {
    return <Host aria-label={this.ariaLabel()}>
      <a href={this.url}>
        <slot name="icon"></slot>
        <slot>
          {this.twitter ? <span>{`${this.twitter}`}</span> : undefined}
          {this.linkedin ? <span>{`${this.linkedin}`}</span> : undefined}
          {this.medium ? <span>{`${this.medium}`}</span> : undefined}
          {this.dev ? <span>{`${this.dev}`}</span> : undefined}
          {this.github ? <span>{`${this.github}`}</span> : undefined}
          {this.fullUrl ? <span>{`${this.fullUrl}`}</span> : undefined}
        </slot>
      </a>
    </Host>
  }
}
