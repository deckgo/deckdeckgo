import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-unpublish',
  styleUrl: 'app-unpublish.scss',
})
export class AppUnpublish {
  render() {
    return (
      <p class="ion-padding-top">
        Please note that currently, the presentations you have shared are not automatically removed from internet. If you wish to un-publish them, drop us a
        message on one of our{' '}
        <a href="https://deckdeckgo.com/en/contact/" rel="noopener norefferer" target="_blank">
          contact
        </a>{' '}
        channels.
      </p>
    );
  }
}
