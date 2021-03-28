import * as React from 'react';
import {Link} from 'gatsby';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';

import {main} from '../../themes/templates/template.module.scss';

const ServicesPage = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <main className={main}>
          <h1>Services</h1>

          <p>
            We aim to be transparent, therefore, furthermore than open sourcing our all code on{' '}
            <a href="http://github.com/deckgo/deckdeckgo" rel="noopener noreferrer">
              Github
            </a>
            , here are the list of services we are using to provide DeckDeckGo.
          </p>

          <h2>Amazon</h2>

          <p>
            We use AWS{' '}
            <a href="https://aws.amazon.com/lambda/" rel="noopener noreferrer">
              Lambda
            </a>
            ,{' '}
            <a href="https://aws.amazon.com/rds/" rel="noopener noreferrer">
              RDS
            </a>
            ,{' '}
            <a href="https://aws.amazon.com/s3/" rel="noopener noreferrer">
              S3
            </a>{' '}
            and{' '}
            <a href="https://aws.amazon.com/sqs/" rel="noopener noreferrer">
              SQS
            </a>{' '}
            to save and publish online the presentations as Progressive Web Apps. The choice behind this is mostly the fact that we thought that the S3 solution
            was a good one for our purpose but beside that, it was also challenging to run Haskell Webapps on AWS Lambda. Our AWS cloud server is set in{' '}
            <samp>us-east-1</samp> (Virginia).
          </p>

          <h2>Google</h2>

          <p>
            We are using{' '}
            <a href="https://firebase.google.com/products/firestore/" rel="noopener noreferrer">
              Firestore
            </a>{' '}
            to save your data and the presentations you are editing. We are also using Google Firebase{' '}
            <a href="https://firebase.google.com/products/hosting/" rel="noopener noreferrer">
              Hosting
            </a>{' '}
            and{' '}
            <a href="https://firebase.google.com/products/auth/" rel="noopener noreferrer">
              Authentication
            </a>
            . Both feature are good match to serve and deploy easily Progressive Web Apps. Their Authentication is also interesting as it provides the social
            login we were looking for (like email and GitHub). Our Firebase cloud server is set in <samp>nam5</samp> (<samp>us-central</samp>).
          </p>

          <h2>Tenor and Unsplash</h2>

          <p>
            To provide a user friendly gifs and stock photos integration we have integrated the APIs provided by{' '}
            <a href="https://tenor.com/" rel="noopener noreferrer">
              Tenor
            </a>
            , which is owned by Google, and{' '}
            <a href="https://unsplash.com/" rel="noopener noreferrer">
              Unsplash
            </a>
            .
          </p>

          <h2>Font Awesome</h2>

          <p>
            The shapes, which could be integrated in your presentation, are free icons provided by{' '}
            <a href="https://fontawesome.com" rel="noopener noreferrer">
              Font Awesome
            </a>
            . We do not use any APIs to fetch these respectively we are hosting them.
          </p>

          <h2>Mailchimp</h2>

          <p>
            In order to send time to time newsletters, mostly when we are releasing new features, we are using{' '}
            <a href="https://mailchimp.com" rel="noopener noreferrer">
              Mailchimp
            </a>
            . Upon creating an account users are opted into it but they can opt out through their account’s “Settings” page and at the link of the footer in any
            of these non-administrative emails.
          </p>

          <p>
            All these services are covered in our <Link to="/privacy">Privacy Policy</Link> and <Link to="/terms">Terms of Services</Link>.
          </p>
        </main>
      </section>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default ServicesPage;
