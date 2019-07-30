import {Component, h} from '@stencil/core';

@Component({
    tag: 'app-faq',
    styleUrl: 'app-faq.scss'
})
export class AppFaq {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content class="ion-padding">

                <main class="ion-padding">
                    <h1>Frequently asked questions</h1>

                    <h3 class="ion-padding-top">Why DeckDeckGo?</h3>

                    <p>To be honest, it's difficult to really shorten what drives us to invest our spare time in DeckDeckGo. Maybe "we are just nerds who like to develop ideas and learn by doing" or maybe "we are just crazy and we underestimated a bit the complexity of this project".</p>

                    <p>Earlier this year (2018) I published an <a href="https://dev.to/daviddalbusco/we-are-developing-an-open-source-editor-for-presentations-1bng" target="_blank">article</a> to present our project and ourselves, you might find more answers in it if you wish to have a more concrete answer.</p>

                    <h3 class="ion-padding-top">Is DeckDeckGo a company's business or a side project?</h3>

                    <p>It's a <strong>side project</strong>, we have developed DeckDeckGo during our spare time, coding it at nights and on weekends.</p>

                    <h3 class="ion-padding-top">Why open source?</h3>

                    <p>"Save the cheerleader, save the world" - The world probably don't need another presentations tool but for sure, open sourcing such a platform won't make it (that) worth.</p>

                    <p>For us, this project is also a learning tool as we are challenging ourselves often with technology and questions we might no have to face in our jobs. Open sourcing it might also help other who would face the same questions, who knows. Or even better, if we are lucky, other might notice our issues and might want to contribute to give us a hand.</p>

                    <p><strong>Sharing is caring</strong></p>

                    <h3 class="ion-padding-top">Why templates? Why can't I do all the styling I wish in my presentations?</h3>

                    <p>We aim to let you create and publish presentations which could be browsed on <strong>any devices</strong>. The slides should fit landscape screens (with a beamer, on a screen or on a tablet) but also portrait screens. Using templates give us a bit of control on how things are going to be displayed regardless of the devices or format or in a responsive way.</p>

                    <p>Moreover, we are not designers. We just try our best to create templates which looks good.</p>

                    <h3 class="ion-padding-top">How could I print? How could I export to PDF?</h3>

                    <p>You can't.</p>

                    <p>With DeckDeckGo each presentations are shared as standalone applications. It's a bit another concept and we think that it is quite interesting to challenge it.</p>

                    <p>Moreover, all trees thank you in advance for not printing your decks.</p>

                    <h3 class="ion-padding-top">I don't want to use the editor, I want to code my presentations, is that possible?</h3>

                    <p>No problemo. This editor use separate Web Components we open sourced too as core engine. Therefore, if you wish to code your own presentations using our libraries, HTML or Markdown, of course you could. Run <strong>npm init deckdeckgo</strong> to get started.</p>

                </main>

            </ion-content>
        ];
    }

}
