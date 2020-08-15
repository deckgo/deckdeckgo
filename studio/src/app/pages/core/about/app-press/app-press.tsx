import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-press',
  styleUrl: 'app-press.scss',
})
export class AppPress {
  render() {
    return [
      <app-navigation presentation={true}></app-navigation>,
      <ion-content class="ion-padding">
        <main class="ion-padding">
          <h1>Press</h1>

          <p>
            We don't provide unfortunately any press kit yet. We are open source and are welcoming any contributions, we would be happy to get your help to
            create such material 😄.
          </p>

          <p>
            That being said, you can download our logo as a{' '}
            <a href="https://deckdeckgo.com/assets/img/deckdeckgo-logo.svg" target="_blank" rel="noopener noreferrer">
              svg
            </a>{' '}
            and could{' '}
            <ion-router-link href="/contact" routerDirection="forward">
              contact
            </ion-router-link>{' '}
            us for any inquiries.
          </p>

          <h1 class="ion-padding-top">Featured Stories</h1>

          <article class="ion-margin-top">
            <a href="https://medium.com/swlh/deckdeckgo-why-building-presentation-with-web-standards-makes-sense-1b6062a84fa" rel="noopener noreferrer">
              Why building presentations with web standards makes sense
            </a>
            <span>
              <small>2020-05-06</small>
            </span>
          </article>

          <article class="ion-margin-top">
            <a href="https://qiita.com/berry-clione/items/8826729673fcdc9bd12f" rel="noopener noreferrer">
              ブラウザ上でスライド作ってプレゼンするDeckDeckGoをローカル環境で構築して脱パワポしてみた
            </a>
            <span>
              <small>2019-10-06</small>
            </span>
          </article>

          <article class="ion-margin-top">
            <a href="https://www.tekcrispy.com/2019/09/25/deckdeckgo/" rel="noopener noreferrer">
              Crea presentaciones con diapositivas rápidamente
            </a>
            <span>
              <small>2019-09-25</small>
            </span>
          </article>

          <article class="ion-margin-top">
            <a href="https://awards.dinacon.ch/en/shortlist-is-out/" rel="noopener noreferrer">
              Shortlisted for the DINAcon Awards 2019
            </a>
            <span>
              <small>2019-09-24</small>
            </span>
          </article>

          <article class="ion-margin-top">
            <a
              href="https://www.moongift.jp/2019/09/deckdeckgo-%E3%83%AA%E3%83%A2%E3%83%BC%E3%83%88%E3%82%B3%E3%83%B3%E3%83%88%E3%83%AD%E3%83%BC%E3%83%AB%E3%82%82%E3%81%A7%E3%81%8D%E3%82%8Bpwa%E3%81%AEhtml%E3%82%B9%E3%83%A9%E3%82%A4%E3%83%89/"
              rel="noopener noreferrer">
              リモートコントロールもできるPWAのHTMLスライド
            </a>
            <span>
              <small>2019-09-01</small>
            </span>
          </article>
        </main>
      </ion-content>,
    ];
  }
}
