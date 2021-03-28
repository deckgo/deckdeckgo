import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';

import {main} from '../../themes/templates/template.module.scss';
import {Link} from 'gatsby';

const FaqPage = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <main className={main}>
          <h1>Frequently asked questions</h1>

          <h3>Why DeckDeckGo?</h3>

          <p>
            To be honest, it's difficult to really shorten what drives us. Maybe "we are just nerds who like to develop ideas and learn by doing" or maybe "we
            are just crazy and we underestimated a bit the complexity of this project" but, in all cases, we love to develop DeckDeckGo.
          </p>

          <p>
            Early 2018, I published an{' '}
            <a
              href="https://dev.to/daviddalbusco/we-are-developing-an-open-source-editor-for-presentations-1bng"
              target="_blank"
              rel="noopener noreferrer"
              style={{textDecoration: 'underline'}}>
              article
            </a>{' '}
            to present our project and ourselves, you might find more answers in it if you wish to have a more concrete answer.
          </p>

          <h3>Why open source?</h3>

          <p>
            "Save the cheerleader, save the world" - The world probably don't need another presentations tool but for sure, open sourcing such a platform won't
            make it (that) worth.
          </p>

          <p>
            For us, this project is also a learning tool as we are challenging ourselves often with technology and questions we might no have to face in our
            jobs. Open sourcing it might also help other who would face the same questions, who knows. Or even better, if we are lucky, other might notice our
            issues and might want to contribute to give us a hand.
          </p>

          <p>Sharing is caring.</p>

          <h3>Why templates? Why can't I do all the styling I wish in my presentations?</h3>

          <p>
            We aim to let you create and publish presentations which could be browsed on <strong>any devices</strong>. The slides should fit landscape screens
            (with a beamer, on a screen or on a tablet) but also portrait screens. Using templates give us a bit of control on how things are going to be
            displayed regardless of the devices or format or in a responsive way.
          </p>

          <p>
            That being said, for our{' '}
            <Link to="/en/enterprise/" style={{textDecoration: 'underline'}}>
              enterprise
            </Link>{' '}
            we do offer templates and options that match their CI/CD. In addition, we are in the making to open our online editor to custom users and community
            templates.
          </p>

          <h3>How could I print? How could I export to PDF?</h3>

          <p>
            You can't (but we do understand that you might wish to if you are a{' '}
            <Link to="/en/enterprise/" style={{textDecoration: 'underline'}}>
              company
            </Link>
            ).
          </p>

          <p>
            With DeckDeckGo each presentations are shared as standalone applications. It's a bit another concept and we think that it is quite interesting to
            challenge it.
          </p>

          <p>Moreover, all trees thank you in advance for not printing your decks.</p>

          <h3>I don't want to use the editor, I want to code my presentations, is that possible?</h3>

          <p>
            No problemo. This editor use separate Web Components we open sourced too as core engine. Therefore, if you wish to code your own presentations using
            our libraries HTML of course you can. Run{' '}
            <span style={{background: 'var(--color-dark)', color: 'var(--color-dark-contrast)', padding: '0 4px', borderRadius: '2px'}}>
              npm init deckdeckgo
            </span>{' '}
            to get started.
          </p>

          <h3>I just scanned a QR code in my presentation and I landed on the homepage, what the heck happened?</h3>

          <p>
            The QR codes you add to your presentations are by default linked with the homepage. As soon as you share them, their content will automatically be
            updated with their online urls. Alternatively, you could also provide a custom url for their content.
          </p>
        </main>
      </section>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default FaqPage;
