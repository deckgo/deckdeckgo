import {errorStore} from '@deckdeckgo/studio';
import {DeckdeckgoPoll, DeckdeckgoPollAnswer} from '@deckdeckgo/types';
import {Component, h, Prop, State} from '@stencil/core';
import {get, set} from 'idb-keyval';
import {PollService} from '../../../services/poll/poll.service';
import i18n from '../../../stores/i18n.store';
import pollStore from '../../../stores/poll.store';
import {renderI18n} from '../../../utils/core/i18n.utils';

@Component({
  tag: 'app-poll',
  styleUrl: 'app-poll.scss'
})
export class AppPoll {
  @Prop({mutable: true})
  pollKey: string;

  @State()
  private choice: string;

  @State()
  private connecting: boolean = false;

  @State()
  private hasVoted: boolean = false;

  @State()
  private keywordIndex: number = Math.floor(Math.random() * 4);

  private keywords: string[] = ['You did it', 'Applause', 'Thumbs up', 'Congratulations'];

  private pollService: PollService;

  private destroyPollListener;

  constructor() {
    this.pollService = PollService.getInstance();
  }

  async componentWillLoad() {
    this.destroyPollListener = pollStore.onChange('poll', (poll: DeckdeckgoPoll | undefined) => {
      if (this.pollKey && (!poll || poll === undefined)) {
        errorStore.default.state.error = 'Oopsie the poll was not found. Double check that the code is correct and try again.';
      }

      this.connecting = false;
    });

    await this.pollService.connect(this.pollKey);

    await this.initHasVoted();
  }

  async disconnectedCallback() {
    await this.pollService.disconnect();

    if (this.destroyPollListener) {
      this.destroyPollListener();
    }
  }

  private async onChoiceChange($event: CustomEvent) {
    this.choice = $event && $event.detail ? $event.detail.value : undefined;
  }

  private async handleSubmit($event: Event) {
    $event.preventDefault();

    if (!pollStore.state.poll || !pollStore.state.poll.key) {
      return;
    }

    if (!this.choice || this.choice === undefined || this.choice === '') {
      return;
    }

    try {
      await this.pollService.vote(pollStore.state.poll.key, this.choice);

      this.hasVoted = true;

      await set(`deckdeckgo_poll_${pollStore.state.poll.key}`, new Date().getTime());
    } catch (err) {
      errorStore.default.state.error = err;
    }
  }

  private async handleSubmitJoin($event: Event) {
    $event.preventDefault();

    if (!this.pollKey || this.pollKey === undefined || this.pollKey === '') {
      return;
    }

    this.connecting = true;

    await this.initHasVoted();

    if (this.hasVoted) {
      return;
    }

    await this.pollService.disconnect();
    await this.pollService.connect(this.pollKey);
  }

  private handlePollKeyInput($event: CustomEvent<KeyboardEvent>) {
    this.pollKey = ($event.target as InputTargetEvent).value;
  }

  private async initHasVoted() {
    if (this.pollKey && this.pollKey !== undefined && this.pollKey !== '') {
      const lastVote: number = await get(`deckdeckgo_poll_${this.pollKey}`);

      // Can't vote for the same poll for two hours
      this.hasVoted = lastVote > 0 && lastVote >= new Date().getTime() - 2 * 60 * 60 * 1000;
    } else {
      this.hasVoted = false;
    }
  }

  private cancel() {
    this.pollKey = undefined;
    this.hasVoted = false;
    pollStore.reset();
  }

  render() {
    return [
      <app-navigation></app-navigation>,
      <ion-content class="ion-padding">
        <main class="ion-padding fit" style={this.hasVoted ? {height: '100%'} : undefined}>
          {this.renderPoll()}
          {this.renderJoinPoll()}
          {this.renderHasVoted()}
        </main>
      </ion-content>
    ];
  }

  private renderPoll() {
    if (this.hasVoted) {
      return undefined;
    }

    if (!pollStore.state.poll || !pollStore.state.poll.poll) {
      return undefined;
    }

    return [
      <h1>{pollStore.state.poll.poll.label}</h1>,
      <form onSubmit={(e: Event) => this.handleSubmit(e)}>
        <ion-list class="ion-padding-top">
          <ion-radio-group onIonChange={($event) => this.onChoiceChange($event)}>{this.renderPollChoices()}</ion-radio-group>
        </ion-list>

        {this.renderSubmitForm()}
      </form>
    ];
  }

  private renderPollChoices() {
    if (!pollStore.state.poll.poll.values || pollStore.state.poll.poll.values.length <= 0) {
      return undefined;
    }

    return pollStore.state.poll.poll.values.map((choice: DeckdeckgoPollAnswer) => {
      return (
        <ion-item>
          <ion-label>{choice.label}</ion-label>
          <ion-radio slot="start" value={choice.key} mode="md"></ion-radio>
        </ion-item>
      );
    });
  }

  private renderJoinPoll() {
    if (this.hasVoted) {
      return undefined;
    }

    if (pollStore.state.poll?.poll) {
      return undefined;
    }

    return [<h1>{i18n.state.poll.vote_now}</h1>, <p>{i18n.state.poll.scan}</p>, this.renderJoinPollForm()];
  }

  private renderJoinPollForm() {
    return (
      <form onSubmit={(e: Event) => this.handleSubmitJoin(e)}>
        <ion-list>
          <ion-item>
            <ion-input
              value={this.pollKey}
              debounce={500}
              minlength={1}
              required={true}
              input-mode="text"
              onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handlePollKeyInput($event)}
            ></ion-input>
          </ion-item>
        </ion-list>

        {this.renderSubmitJoinPollForm()}
      </form>
    );
  }

  private renderSubmitJoinPollForm() {
    return (
      <ion-button
        type="submit"
        class="ion-margin-top"
        disabled={!this.pollKey || this.pollKey === undefined || this.pollKey === '' || this.connecting}
        color="primary"
        shape="round"
      >
        <ion-label>{i18n.state.core.submit}</ion-label>
      </ion-button>
    );
  }

  private renderSubmitForm() {
    return (
      <div class="submit-form ion-margin-top">
        <ion-button type="submit" disabled={!this.choice || this.choice === undefined || this.choice === ''} color="primary" shape="round">
          <ion-label>{i18n.state.core.submit}</ion-label>
        </ion-button>
        <a class="ion-margin-start" onClick={() => this.cancel()}>
          <ion-label>{i18n.state.core.cancel}</ion-label>
        </a>
      </div>
    );
  }

  private renderHasVoted() {
    if (!this.hasVoted) {
      return undefined;
    }

    return (
      <article>
        <app-random-gif keyword={this.keywords[this.keywordIndex]}></app-random-gif>

        <h1 class="ion-text-center">
          {this.keywords[this.keywordIndex]}! {i18n.state.poll.vote_cast}
        </h1>

        <p class="ion-text-center">{i18n.state.poll.enjoy}</p>

        <ion-button type="button" onClick={() => this.cancel()} color="primary" shape="round" class="ion-margin-top">
          <ion-label>{i18n.state.poll.vote_again}</ion-label>
        </ion-button>

        <div class="by-deckdeckgo">
          {renderI18n(i18n.state.poll.created_with, {
            placeholder: '{0}',
            value: (
              <ion-router-link href="/" routerDirection="forward">
                <div>
                  <app-logo></app-logo> DeckDeckGo
                </div>
              </ion-router-link>
            )
          })}
        </div>
      </article>
    );
  }
}
