import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-remote',
  styleUrl: 'app-remote.scss',
})
export class AppRemote {
  render() {
    return [
      <app-navigation presentation={true}></app-navigation>,
      <ion-content class="ion-padding">
        <main class="ion-padding">
          <h1>Remote control</h1>

          <h3 class="ion-padding-top">Use your phone to control your presentations and access your speaker notes.</h3>

          <p class="ion-padding-top">
            Out of the box, without any special hardware or configuration, any presentations could be remote controlled with our “remote control” application.
          </p>

          <p>It handles actions like swiping slides, drawing on the presentation and even offers a countdown feature.</p>

          <p>Moreover the content of your slides and your speaker notes are synchronised and displayed in the remote control.</p>

          <p>We are also taking advantages of QR codes in order to establish easily the connection between the controller and the decks.</p>

          <p class="ion-padding-bottom">
            Start the{' '}
            <a href="https://deckdeckgo.app" target="_blank" rel="noopener noreferrer">
              remote control
            </a>{' '}
            now 🚀
          </p>

          <iframe width="260" height="146" src="https://www.youtube.com/embed/PnSNT5WpauE"></iframe>
        </main>
      </ion-content>,
    ];
  }
}
