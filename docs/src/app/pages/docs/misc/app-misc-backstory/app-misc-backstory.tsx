import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-misc-backstory',
})
export class AppMiscBackstory {
  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-misc-backstory-backstory">Backstory</h1>
          <p>
            <em>As backstory, here a small article I wrote in Mai 2019 when we were developing the DeckDeckGo editor and platform.</em>
          </p>
          <p>
            <img src="https://cdn-images-1.medium.com/max/2600/1*zuqx73hjAWNnmwRpjJ06Zw.jpeg" alt="" />
            <span class="figcaption_hack">
              <em>Lisbon is a beautiful city. Hopefully, I’m a better engineer than a photographer</em> 😅
            </span>
          </p>
          <p>
            I’m currently sitting in a plane between Lisbon and Zürich, back from a family trip, and I thought that I could use my travel time to write a
            non-technical blog post (something new to me) about our project <a href="https://deckdeckgo.com">DeckDeckGo</a>, the open source editor for
            presentations. In this post I’ll try to summarize who we are, why we are developing this project and where do we stand in the development’s
            progress.
          </p>
          <p>
            I’m not sure this post will ever actually interest anyone, but well, I think that at least it will makes my travel faster. Moreover, my mum is
            currently reading a novel next to me and I also don’t want to disturb her, it’s a win-win situation 😉
          </p>
          <p>
            I also write these lines because I just finished to read the <a href="http://makebook.io">MAKE</a> book of{' '}
            <a href="https://twitter.com/levelsio">Pieter Levels</a> which was really interesting. His “indie” point of view on the startup “world” seems honest
            and quite refreshing to me. I could definitely advice you to acquire it.
          </p>
          <h3 id="app-misc-backstory-introduction">Introduction</h3>
          <p>First thing first, I should probably explain what’s DeckDeckGo 😆</p>
          <p>
            <strong>DeckDeckGo aims to be the open source editor for PWA presentations.</strong>
          </p>
          <p>What does that mean?</p>
          <p>
            It means that every presentations you will write and publish with DeckDeckGo, are going to be apps (= Progressive Web Apps) too. Basically, we are
            developing an app to let you write decks of slides which are going to be published online as apps too (I hope that’s “meta” enough for you 😉).
          </p>
          <p>
            Furthermore, we thought that it would be cool if our project wasn’t “just” an editor but also an online community for sharing presentations, slides
            and talks about your interests and ideas.
          </p>
          <p>
            <em>The flight attendants is passing by with sandwiches, I’ll continue the article soon … and I’m back</em>
          </p>
          <p>
            I want also to emphasis something else, which might be given for some, but which is important to us: DeckDeckGo is <strong>open source</strong>. We
            think that you can’t really hope to have a chance to develop a community without being transparent and without sharing. We also hope that by
            following this path, we are going to become better programmers.
          </p>
          <h3 id="app-misc-backstory-who">Who</h3>
          <p>
            We, <a href="https://twitter.com/nasmattia">Nicolas Mattia</a> and I, are developing DeckDeckGo. Instead of telling you in a common “biography” way
            who we are, I thought that trying to summarize the first time we met at the <a href="http://zurich.impacthub.ch">Impact Hub Zürich</a> (a great
            co-working space in Zürich) would just speak by itself. Let me try to summarize this very first discussion:
          </p>
          <ul>
            <li>Hey, I’m David, I come from the French-speaking area of Switzerland</li>
            <li>Hey, I’m Nicolas, I come from the French-speaking part too</li>
          </ul>
          <p>
            <em>Obviously at that point we switched to french 😜</em>
          </p>
          <ul>
            <li>No way, I come from Jura</li>
            <li>Really, I’m from Neuchâtel, that’s next door</li>
            <li>How cool is that. What do you do?</li>
            <li>
              I weird backend stuffs using <a href="https://www.haskell.org">Haskell</a>, like wizardry, it’s the future. And you?
            </li>
          </ul>
          <p>
            <em>Actually he didn’t exactly said that, it’s just…I don’t fully understand the black magic Nicolas is using 🤣</em>
          </p>
          <ul>
            <li>
              I’m a fullstack freelancer/project manager but currently I mostly develop apps using <a href="https://ionicframework.com">Ionic</a> and{' '}
              <a href="http://angular.io">Angular</a>
            </li>
            <li>Nice. Let’s build something together one of these days</li>
            <li>Definitely</li>
          </ul>
          <p>
            Fast forward, Nicolas spent some months abroad working remotely, meanwhile I started DeckDeckGo and finally two months ago, while we were having
            dinner, I “convinced” him to jump into the project and we started to brainstorm and develop all the new cool ideas we had together 🚀
          </p>
          <h3 id="app-misc-backstory-why">Why</h3>
          <p>
            In October 2018 I had the opportunity to give a talk about Web Components at the{' '}
            <a href="https://www.meetup.com/fr-FR/Pantalks-tech-non-tech-talks-Panter-AG-Zurich/events/255430094/">Pantalks</a>. When I was preparing my
            presentation, it hits me that wasn’t actually using the technology I was supposed to demonstrate in order to develop my slides. That’s why I decided
            to “quickly” build a small new library to support my presentation.
          </p>
          <p>
            I could have stop there but there is something really interesting with presentations: everyone has written a presentation once in her/his life and
            everyone is super creative too. Therefore, each times I spoke about “my pet project” with someone, new crazy and awesome ideas were emerging.
          </p>
          <p>
            From there it was really easy for me to go a bit “out of control” (like I like to say). I just began to develop all these features like being able
            to remote control any presentations using WebRTC, or being able to add easily charts, or being able to easily add GIFs, YouTube videos or even QR
            codes and more.
          </p>
          <p>
            Time passed “but” the project remains a frontend project for techies as, mostly, I didn’t wanted to make alone the (big) step of turning this
            “small” library into a full product, until fortunately, Nicolas decided to join me 🤟
          </p>
          <h3 id="app-misc-backstory-where">Where</h3>
          <p>
            Where do we stand? Well firstly, I’ve to admit that we are maybe a bit behind our very first schedule. First time we discussed about planing, I
            clearly remember Nicolas telling me something like “Easy, such a backend solution, 2–3 days tops” 😂
          </p>
          <p>
            For our defense, I’ve to add that since that day we also had a lot of new ideas, which we were agree to develop, as for us, DeckDeckGo is as much a
            learning tool as a fun project to develop.
          </p>
          <p>
            Furthermore, we have both other real main activities. I’m a freelancer dedicate to my client’s projects and Nicolas is employee. That let us
            evenings, late nights, super early mornings and weekends to nerd DeckDeckGo.
          </p>
          <p>
            <em>
              I have to shutdown the computer, we are about to land … and I’m back. We landed, I said bye to my family and now I’m in the train between Basel
              and Zürich. Also, welcome back to Switzerland, 12.10 CHF (around 12$) for a ham sandwich and a bottle of water 🙈
            </em>
          </p>
          <p>
            That being said, we are both aware that “we have to unleash a beta soon” and that we should focus on core features in order to be able to ship
            soonish a MVP (= minimum viable product). To achieve this goal, we think that the following are the missing features we have still to develop:
          </p>
          <ul>
            <li>Publication: collect the user slides, pack everything with the existing starter kit and unleash the decks online as PWAs</li>
            <li>Media: we don’t have yet developed the ability to upload images in any slides</li>
            <li>Feed: automatically generate the social card and feed layout of each presentations</li>
          </ul>
          <p>
            Therefore, without any pressure, our actual goal is to expose DeckDeckGo to its first tests IRL somewhere in June and we hope to start straight
            afterwards our “beta program” for more extended tests and, hopefully, gather a couple of users’ feedbacks and maybe even better, let the first users
            write their presentations “for real” 🤞
          </p>
          <h3 id="app-misc-backstory-cherry-on-the-cake-🍒🎂">Cherry on the cake 🍒🎂</h3>
          <p>
            DeckDeckGo, I guess you get it, is open source, therefore, if you wish to track our progress or wish to already contribute to our “small” pet
            project, you could find us on <a href="https://github.com/deckgo/deckdeckgo">Github</a> .
          </p>
          <p>
            Finally, if you would like to help us shape a cool presentation platform and editor by joining our{' '}
            <a href="https://deckdeckgo.com/?index=2">beta program</a>, that would be awesome. We would love to have you on board and to hear from you, “join
            the deck side” 😉
          </p>
          <p>
            <em>
              It’s now 21:25 and I’m finally reaching out Zürich. Not sure someone will ever read these final lines but this article was definitely useful to
              me, I totally lost the track of the travel time and it just felt like a couple of minutes. Might do the exercise again in the future.
            </em>
          </p>
          <p>To infinity and beyond 🖖</p>
          <p>
            <a href="https://twitter.com/daviddalbusco">David</a>
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
