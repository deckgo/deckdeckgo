import {Component, Method, Prop, State, Watch, h} from '@stencil/core';

import {DeckdeckgoExtra} from '../deckdeckgo-extra';

@Component({
  tag: 'deckgo-social',
  styleUrl: 'deckdeckgo-social.scss',
  shadow: true
})
export class DeckdeckgoSocial implements DeckdeckgoExtra {

  @State() url: string;

  @Prop({reflect: true}) twitter: string;
  @Prop({reflect: true}) linkedin: string;
  @Prop({reflect: true}) medium: string;
  @Prop({reflect: true}) dev: string;
  @Prop({reflect: true}) github: string;
  @Prop({reflect: true}) fullUrl: string;

  componentDidLoad() {
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

    this.url = 'https://dev.to/' + this.dev ;
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
