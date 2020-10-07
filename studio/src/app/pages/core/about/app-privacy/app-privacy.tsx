import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-privacy',
  styleUrl: 'app-privacy.scss',
})
export class AppAbout {
  render() {
    return [
      <app-navigation presentation={true}></app-navigation>,
      <ion-content class="ion-padding">
        <main class="ion-padding">
          <h1>Privacy Policy</h1>

          <p>Effective 3rd July 2019</p>

          <p>This policy covers DeckDeckGo.</p>

          <h2>What information DeckDeckGo collects and Why</h2>

          <p>
            In order to give you the best possible experience using DeckDeckGo, we collect information from your interactions with our network. We use common
            internet technologies, such as cookies and web server logs. We collect this basic information from everybody, whether they have an account or not.
          </p>

          <p>The information we collect about all visitors to our website includes:</p>

          <ul class="ion-padding-start">
            <li>The visitor’s browser type</li>
            <li>Referring site</li>
            <li>The date and time of each visitor request</li>
            <li>We also collect potentially personally-identifying information like Internet Protocol (IP) addresses.</li>
          </ul>

          <p>We use this information to:</p>

          <ul class="ion-padding-start">
            <li>Provide, test, improve, promote and personalize DeckDeckGo Services</li>
            <li>Fight spam and other forms of abuse</li>
            <li>Generate aggregate, non-identifying information about how people use DeckDeckGo Services</li>
          </ul>

          <p>
            In order for you to create an account on DeckDeckGo and use our Services, we need to collect and process certain information. Depending on your use
            of the Services, that may include:
          </p>

          <ul class="ion-padding-start">
            <li>Communications you send to us (for example, when you ask for support, send us questions or comments, or report a problem);</li>
            <li>Information that you submit on or to DeckDeckGo in the form of reactions, comments, or messages to other users;</li>
            <li>The email address you would submit, if you choose to sign up using "Sign in with email".</li>
            <li>
              The email address associated with your Google account, if you choose to sign up using your Google credentials. DeckDeckGo will also request
              permission to access additional information (these permissions are governed by Google’s privacy policies and can be managed through your Google
              privacy settings). We never post anything to your Google without your permission.
            </li>
            <li>
              The email address associated with your GitHub account, if you choose to sign up using your Github credentials. DeckDeckGo will also request
              permission to access additional information (these permissions are governed by GitHub’s privacy policies and can be managed through your GitHub
              privacy settings). We never post anything to your GitHub without your permission.
            </li>
            <li>You also have the option to give us more information if you want to, and this may include “User Personal Information.”</li>
          </ul>

          <h2>Information Disclosure</h2>

          <p>We do not share, sell, rent, or trade User Personal Information with third parties for commercial purposes.</p>

          <p>
            We may share certain aggregated, non-personally identifying information with others about how our users, collectively, use DeckDeckGo. For example,
            we may share information pertaining to the popularity of different category of presentations.
          </p>

          <h2>Third Party Vendors</h2>

          <h3>Data Storage</h3>

          <p>
            DeckDeckGo uses third-party vendors and hosting partners for hardware, software, networking, storage, and related technology we need to run
            DeckDeckGo. By using DeckDeckGo Services, you authorize DeckDeckGo to transfer, store, and use your information in the United States and any other
            country where we operate. All service providers and third-party vendors are required to meet our data protection standards.
          </p>

          <h3>Site monitoring </h3>

          <p>
            DeckDeckGo uses a variety of third-party services to diagnose errors and improve the performance of our site. We aim to minimize the amount of
            personal information shared, but the information may include your IP address or other identifying information. All service providers and third-party
            vendors are required to meet our data protection standards.
          </p>

          <h3>Third-Party Embeds</h3>

          <p>
            Some of the content that you see displayed on DeckDeckGo is not hosted by DeckDeckGo. These “embeds” are hosted by a third-party and embedded in
            DeckDeckGo. For example: YouTube videos, Giphy GIFs, Twitter tweets, or GitHub code that appear within a DeckDeckGo presentation. These files send
            data to the hosted site just as if you were visiting that site directly. DeckDeckGo does not control what data third parties collect in cases like
            this, or what they will do with it. Third-party embeds on DeckDeckGo are not covered by this privacy policy; they are covered by the privacy policy
            of the third-party service. Be mindful when interacting with these services.
          </p>

          <h3>Tracking & Cookies</h3>

          <p>
            We use browser cookies and similar technologies to recognize you when you return to our Services. Third-party vendors may also use cookies for
            various reasons.
          </p>

          <h3>Data Security</h3>

          <p>
            We use encryption (HTTPS/TLS) to protect data transmitted to and from our site. However, no data transmission over the Internet is 100% secure, so
            we can’t guarantee security. You use the Service at your own risk, and you’re responsible for taking reasonable measures to secure your account.
          </p>

          <h3>Administrative Emails from DeckDeckGo</h3>

          <p>
            Sometimes we’ll send you emails about your account, service changes or new policies. You can’t opt out of this type of “transactional” email (unless
            you delete your account).{' '}
          </p>

          <p>
            When you interact with a transactional email sent from DeckDeckGo (such as opening an email or clicking on a particular link in an email), we may
            receive information about that interaction.
          </p>

          <h3>Non-administrative Emails from DeckDeckGo</h3>

          <p>
            Upon creating a DeckDeckGo account, you will be opted into the DeckDeckGo Newsletter and other non-administrative email. Your email address and user
            profile information may be stored by a third-party email provider such as MailChimp or Sendgrid. You can opt out of non-administrative emails such
            as digests, newsletters, and activity notifications through your account’s “Settings” page and at the link of the footer in any non-administrative
            email you receive from us.
          </p>

          <p>
            When you interact with a non-administrative email sent from DeckDeckGo (such as opening an email or clicking on a particular link in an email), we
            may receive information about that interaction.
          </p>

          <h3>Deleting Your Personal Information</h3>

          <p>You may request deletion of your personal information and account by emailing hello@deckdeckgo.com.</p>

          <p>
            To protect information from accidental or malicious destruction, we may maintain residual copies for a brief time period. But, if you delete your
            account, your information and content will be unrecoverable after that time.
          </p>

          <h3>Data Portability</h3>

          <p>If you would like to request a copy of your user data, please email hello@deckdeckgo.com.</p>

          <h3>Business Transfers</h3>

          <p>
            If we are involved in a merger, acquisition, bankruptcy, reorganization or sale of assets such that your information would be transferred or become
            subject to a different privacy policy, we’ll notify you in advance of any such change.
          </p>

          <h3>Changes to this Policy</h3>

          <p>
            We reserve the right to revise this Privacy Policy at any time. If we change this Privacy Policy in the future, we will post the revised Privacy
            Policy and update the “Effective Date,” above, to reflect the date of the changes.
          </p>

          <h3>Questions</h3>

          <p>We welcome feedback about this policy at hello@deckdeckgo.com.</p>
        </main>
      </ion-content>,
    ];
  }
}
