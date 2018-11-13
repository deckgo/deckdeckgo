import {Component, Prop, State, Watch} from '@stencil/core';


@Component({
  tag: 'deckgo-social',
  styleUrl: 'deckdeckgo-social.scss',
  shadow: true
})
export class DeckdeckgoSocial {

  @State() url: string;

  @Prop() twitter: string;
  @Prop() linkedin: string;
  @Prop() medium: string;
  @Prop() github: string;
  @Prop() fullUrl: string;

  componentDidLoad() {
    this.concatTwitterUrl();
    this.concatLinkedinUrl();
    this.concatMediumUrl();
    this.concatGithubUrl();
    this.concatFullUrl();
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

  render() {
    return <a href={this.url}>
      <slot name="icon"></slot>
      <slot></slot>
    </a>
  }
}
