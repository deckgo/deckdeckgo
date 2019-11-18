import {Component, Element, h, Listen, State} from '@stencil/core';

@Component({
    tag: 'app-poll-options',
    styleUrl: 'app-poll-options.scss'
})
export class AppPollOptions {

    @Element() el: HTMLElement;

    @State()
    private question: string;

    @State()
    private answers: string[];

    @State()
    private valid: boolean = false;

    componentWillLoad() {
        this.answers = Array.from({length: 3}, (_v, _i) => '');

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
        if (!this.valid) {
            return;
        }

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
                return answer !== ''
            });

            resolve(atLeastOneAnswer !== -1);
        });
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="quinary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Poll</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding">
                <ion-list class="inputs-list">
                    <h2>Question</h2>
                    <ion-item>
                        <ion-input value={this.question} placeholder="Enter your question" debounce={500}
                                   onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleQuestionInput(e)}></ion-input>
                    </ion-item>

                    <h2>Answers</h2>
                    {this.renderAnswers()}

                    <div class="add-answer">
                        <ion-button fill="clear" color="medium" onClick={() => this.addAnswer()}>
                            <ion-icon name="add" slot="start"></ion-icon>
                            <ion-label>Add an answer</ion-label>
                        </ion-button>
                    </div>
                </ion-list>

                <ion-button
                    disabled={!this.valid}
                    color="dark" shape="round" onClick={() => this.save()}>
                    <ion-label>Save</ion-label>
                </ion-button>
            </ion-content>
        ];
    }

    private renderAnswers() {
        return (
            this.answers.map((answer: string, i: number) => {
                return <ion-item>
                    <ion-input value={answer} placeholder={`Enter answer ${i + 1}`} debounce={500}
                               onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleAnswerInput(e, i)}></ion-input>
                </ion-item>
            })
        );
    }

}
