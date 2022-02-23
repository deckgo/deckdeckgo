import {Component, Element, EventEmitter, h, Listen, Prop, State} from '@stencil/core';
import {AppIcon} from '../../../components/core/app-icon/app-icon';
import i18n from '../../../stores/i18n.store';

@Component({
  tag: 'app-poll-options',
  styleUrl: 'app-poll-options.scss'
})
export class AppPollOptions {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

  @Prop()
  slideDidChange: EventEmitter<HTMLElement>;

  @State()
  private question: string;

  @State()
  private answers: string[];

  @State()
  private valid: boolean = false;

  @State()
  private editDisabled: boolean = false;

  async componentWillLoad() {
    this.editDisabled = await this.isPollAnswered();

    this.question = await this.initQuestion();
    this.answers = await this.initAnswers();

    this.valid = await this.isValid();
  }

  private isPollAnswered(): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      if (!this.selectedTarget) {
        resolve(false);
        return;
      }

      if (typeof (this.selectedTarget as any).isAnswered === 'function') {
        const result: boolean = await (this.selectedTarget as any).isAnswered();
        resolve(result);
        return;
      }

      resolve(true);
    });
  }

  private initQuestion(): Promise<string | undefined> {
    return new Promise<string | undefined>((resolve) => {
      if (!this.selectedTarget || this.selectedTarget === undefined) {
        resolve(undefined);
        return;
      }

      const slot: HTMLElement = this.selectedTarget.querySelector(":scope > [slot='question']");

      resolve(!slot ? undefined : slot.innerHTML);
    });
  }

  private initAnswers(): Promise<string[]> {
    return new Promise<string[]>((resolve) => {
      if (!this.selectedTarget || this.selectedTarget === undefined) {
        resolve(this.initEmptyAnswers());
        return;
      }

      const slots: NodeListOf<HTMLElement> = this.selectedTarget.querySelectorAll(':scope > [slot]');

      if (!slots || slots.length <= 0) {
        resolve(this.initEmptyAnswers());
        return;
      }

      const answers: string[] = Array.from(slots)
        .filter((slot: HTMLElement) => {
          return slot.hasAttribute('slot') && slot.getAttribute('slot').indexOf('answer') > -1;
        })
        .map((slot: HTMLElement) => {
          return slot.innerHTML;
        });

      resolve(answers);
    });
  }

  private initEmptyAnswers(): string[] {
    return Array.from({length: 2}, (_v, _i) => '');
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  async save() {
    if (!this.valid || this.editDisabled) {
      return;
    }

    await this.updateSlide();

    const filterAnswers: string[] = this.answers.filter((answer: string) => {
      return answer !== '';
    });

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss({
      question: this.question,
      answers: filterAnswers
    });
  }

  private async handleQuestionInput($event: CustomEvent<KeyboardEvent>) {
    this.question = ($event.target as InputTargetEvent).value;

    this.valid = await this.isValid();
  }

  private async handleAnswerInput($event: CustomEvent<KeyboardEvent>, index: number) {
    this.answers[index] = ($event.target as InputTargetEvent).value;

    this.valid = await this.isValid();
  }

  private addAnswer(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.answers = [...this.answers, ''];

      resolve();
    });
  }

  private isValid(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (this.question === undefined || !this.question || this.question === '') {
        resolve(false);
        return;
      }

      if (!this.answers || this.answers.length <= 0) {
        resolve(false);
        return;
      }

      const atLeastOneAnswer: number = this.answers.findIndex((answer: string) => {
        return answer !== '';
      });

      resolve(atLeastOneAnswer !== -1);
    });
  }

  private updateSlide(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget || this.selectedTarget === undefined || !document) {
        resolve(undefined);
        return;
      }

      const slots: NodeListOf<HTMLElement> = this.selectedTarget.querySelectorAll(':scope > [slot]');

      if (!slots || slots.length <= 0) {
        resolve();
        return;
      }

      const promises: Promise<void>[] = [];
      promises.push(this.updateSlot('question', 'h1', this.question));

      if (this.answers && this.answers.length > 0) {
        this.answers.forEach((answer: string, i: number) => {
          promises.push(this.updateSlot(`answer-${i + 1}`, 'h2', answer));
        });
      }

      await Promise.all(promises);

      this.slideDidChange.emit(this.selectedTarget);

      resolve();
    });
  }

  private updateSlot(slotName: string, elementName: string, value: string): Promise<void> {
    return new Promise<void>((resolve) => {
      const slot: HTMLElement = this.selectedTarget.querySelector(`:scope > [slot=\'${slotName}\']`);
      if (!slot) {
        if (value && value !== undefined && value !== '') {
          const newSlot: HTMLElement = document.createElement(elementName);
          newSlot.setAttribute('slot', slotName);
          newSlot.innerHTML = value;

          this.selectedTarget.appendChild(newSlot);
        }
      } else {
        if (value && value !== undefined && value !== '') {
          slot.innerHTML = value;
        } else {
          this.selectedTarget.removeChild(slot);
        }
      }

      resolve();
    });
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="senary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
              <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">{i18n.state.menu.poll}</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <ion-list class="inputs-list">
          <h2>{i18n.state.editor.question}</h2>
          <ion-item>
            <ion-input
              value={this.question}
              placeholder={i18n.state.editor.enter_question}
              debounce={500}
              disabled={this.editDisabled}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleQuestionInput(e)}
            ></ion-input>
          </ion-item>

          <h2>Answers</h2>
          {this.renderAnswers()}

          <div class="add-answer">
            <ion-button fill="clear" color="medium" onClick={() => this.addAnswer()} disabled={this.editDisabled}>
              <AppIcon name="add" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
              <ion-label>{i18n.state.editor.add_answer}</ion-label>
            </ion-button>
          </div>
        </ion-list>

        <ion-button disabled={!this.valid || this.editDisabled} color="dark" shape="round" onClick={() => this.save()}>
          <ion-label>{i18n.state.core.save}</ion-label>
        </ion-button>
      </ion-content>
    ];
  }

  private renderAnswers() {
    return this.answers.map((answer: string, i: number) => {
      return (
        <ion-item>
          <ion-input
            value={answer}
            placeholder={`Enter answer ${i + 1}`}
            debounce={500}
            disabled={this.editDisabled}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleAnswerInput(e, i)}
          ></ion-input>
        </ion-item>
      );
    });
  }
}
