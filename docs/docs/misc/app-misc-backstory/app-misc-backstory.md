# Backstory

_As backstory, here a small article I wrote in Mai 2019 when we were developing the DeckDeckGo editor and platform._

![](https://cdn-images-1.medium.com/max/2600/1*zuqx73hjAWNnmwRpjJ06Zw.jpeg)
<span class="figcaption_hack">_Lisbon is a beautiful city. Hopefully, I’m a better engineer than a photographer_ 😅</span>

I’m currently sitting in a plane between Lisbon and Zürich, back from a family trip, and I thought that I could use my travel time to write a non-technical blog post (something new to me) about our project [DeckDeckGo](https://deckdeckgo.com), the open source editor for presentations. In this post I’ll try to summarize who we are, why we are developing this project and where do we stand in the development’s progress.

I’m not sure this post will ever actually interest anyone, but well, I think that at least it will makes my travel faster. Moreover, my mum is currently reading a novel next to me and I also don’t want to disturb her, it’s a win-win situation 😉

I also write these lines because I just finished to read the [MAKE](http://makebook.io) book of [Pieter Levels](https://twitter.com/levelsio) which was really interesting. His “indie” point of view on the startup “world” seems honest and quite refreshing to me. I could definitely advice you to acquire it.

### Introduction

First thing first, I should probably explain what’s DeckDeckGo 😆

**DeckDeckGo aims to be the open source editor for PWA presentations.**

What does that mean?

It means that every presentations you will write and publish with DeckDeckGo, are going to be apps (= Progressive Web Apps) too. Basically, we are developing an app to let you write decks of slides which are going to be published online as apps too (I hope that’s “meta” enough for you 😉).

Furthermore, we thought that it would be cool if our project wasn’t “just” an editor but also an online community for sharing presentations, slides and talks about your interests and ideas.

_The flight attendants is passing by with sandwiches, I’ll continue the article
soon … and I’m back_

I want also to emphasis something else, which might be given for some, but which is important to us: DeckDeckGo is **open source**. We think that you can’t really hope to have a chance to develop a community without being transparent and without sharing. We also hope that by following this path, we are going to become better programmers.

### Who

We, [Nicolas Mattia](https://twitter.com/nasmattia) and I, are developing DeckDeckGo. Instead of telling you in a common “biography” way who we are, I thought that trying to summarize the first time we met at the [Impact Hub Zürich](http://zurich.impacthub.ch) (a great co-working space in Zürich) would just speak by itself. Let me try to summarize this very first discussion:

- Hey, I’m David, I come from the French-speaking area of Switzerland
- Hey, I’m Nicolas, I come from the French-speaking part too

_Obviously at that point we switched to french 😜_

- No way, I come from Jura
- Really, I’m from Neuchâtel, that’s next door
- How cool is that. What do you do?
- I weird backend stuffs using [Haskell](https://www.haskell.org), like wizardry, it’s the future. And you?

_Actually he didn’t exactly said that, it’s just…I don’t fully understand the
black magic Nicolas is using 🤣_

- I’m a fullstack freelancer/project manager but currently I mostly develop apps
  using [Ionic](https://ionicframework.com) and [Angular](http://angular.io)
- Nice. Let’s build something together one of these days
- Definitely

Fast forward, Nicolas spent some months abroad working remotely, meanwhile I started DeckDeckGo and finally two months ago, while we were having dinner, I “convinced” him to jump into the project and we started to brainstorm and develop all the new cool ideas we had together 🚀

### Why

In October 2018 I had the opportunity to give a talk about Web Components at the [Pantalks](https://www.meetup.com/fr-FR/Pantalks-tech-non-tech-talks-Panter-AG-Zurich/events/255430094/). When I was preparing my presentation, it hits me that wasn’t actually using the technology I was supposed to demonstrate in order to develop my slides. That’s why I decided to “quickly” build a small new library to support my presentation.

I could have stop there but there is something really interesting with presentations: everyone has written a presentation once in her/his life and everyone is super creative too. Therefore, each times I spoke about “my pet project” with someone, new crazy and awesome ideas were emerging.

From there it was really easy for me to go a bit “out of control” (like I like to say). I just began to develop all these features like being able to remote control any presentations using WebRTC, or being able to add easily charts, or being able to easily add GIFs, YouTube videos or even QR codes and more.

Time passed “but” the project remains a frontend project for techies as, mostly, I didn’t wanted to make alone the (big) step of turning this “small” library into a full product, until fortunately, Nicolas decided to join me 🤟

### Where

Where do we stand? Well firstly, I’ve to admit that we are maybe a bit behind our very first schedule. First time we discussed about planing, I clearly remember Nicolas telling me something like “Easy, such a backend solution, 2–3 days tops” 😂

For our defense, I’ve to add that since that day we also had a lot of new ideas, which we were agree to develop, as for us, DeckDeckGo is as much a learning tool as a fun project to develop.

Furthermore, we have both other real main activities. I’m a freelancer dedicate to my client’s projects and Nicolas is employee. That let us evenings, late nights, super early mornings and weekends to nerd DeckDeckGo.

_I have to shutdown the computer, we are about to land … and I’m back. We landed, I said bye to my family and now I’m in the train between Basel and Zürich. Also, welcome back to Switzerland, 12.10 CHF (around 12\$) for a ham sandwich and a bottle of water 🙈_

That being said, we are both aware that “we have to unleash a beta soon” and that we should focus on core features in order to be able to ship soonish a MVP (= minimum viable product). To achieve this goal, we think that the following are the missing features we have still to develop:

- Publication: collect the user slides, pack everything with the existing starter kit and unleash the decks online as PWAs
- Media: we don’t have yet developed the ability to upload images in any slides
- Feed: automatically generate the social card and feed layout of each presentations

Therefore, without any pressure, our actual goal is to expose DeckDeckGo to its first tests IRL somewhere in June and we hope to start straight afterwards our “beta program” for more extended tests and, hopefully, gather a couple of users’ feedbacks and maybe even better, let the first users write their presentations “for real” 🤞

### Cherry on the cake 🍒🎂

DeckDeckGo, I guess you get it, is open source, therefore, if you wish to track our progress or wish to already contribute to our “small” pet project, you could find us on [Github](https://github.com/deckgo/deckdeckgo) .

Finally, if you would like to help us shape a cool presentation platform and editor by joining our [beta program](https://deckdeckgo.com/?index=2), that would be awesome. We would love to have you on board and to hear from you, “join the deck side” 😉

_It’s now 21:25 and I’m finally reaching out Zürich. Not sure someone will ever read these final lines but this article was definitely useful to me, I totally lost the track of the travel time and it just felt like a couple of minutes. Might do the exercise again in the future._

To infinity and beyond 🖖

[David](https://twitter.com/daviddalbusco)

[deckdeckgo]: https://deckdeckgo.com
