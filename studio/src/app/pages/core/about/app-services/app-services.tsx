import {Component, h} from '@stencil/core';

@Component({
    tag: 'app-services',
    styleUrl: 'app-services.scss'
})
export class AppAbout {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content class="ion-padding">
                <main class="ion-padding">

                    <h1>Services</h1>

                    <p>We aim to be transparent, therefore, furthermore than open sourcing our all code on <a href="http://github.com/deckgo/deckdeckgo">Github</a>, here are the list of services we are using to provide DeckDeckGo.</p>

                    <h2>Amazon</h2>

                    <p>We use AWS <a href="https://aws.amazon.com/lambda/">Lambda</a>, <a href="https://aws.amazon.com/dynamodb/">DynamoDB</a>, <a href="https://aws.amazon.com/rds/">RDS</a>, <a href="https://aws.amazon.com/s3/">S3</a> and <a href="https://aws.amazon.com/sqs/">SQS</a> to save and publish the presentations as Progressive Web Apps online. The choice behind this is mostly the fact that we thought that the S3 solution was a good one for our purpose but beside that, it was also challenging to run Haskell Webapps on AWS Lambda.</p>

                    <h2>Google</h2>

                    <p>We are using <a href="https://firebase.google.com/products/firestore/">Firestore</a> to save your data and the presentations you are editing. We are also using Google Firebase <a href="https://firebase.google.com/products/hosting/">Hosting</a> and <a href="https://firebase.google.com/products/auth/">Authentication</a>. Both feature are good match to serve and deploy easily Progressive Web Apps. Their Authentication is also interesting as it provides the social login we were looking for (like Email and Github).</p>

                    <h2>Tenor and Unsplash</h2>

                    <p>To provide a user friendly gifs and stock photos integration we have integrated the APIs provided by <a href="https://tenor.com/">Tenor</a>, which is owned by Google, and <a href="https://unsplash.com/">Unsplash</a>.</p>

                    <p>All these services are covered in our <ion-router-link href="/privacy" routerDirection="forward">Privacy Policy</ion-router-link> and <ion-router-link href="/terms" routerDirection="forward">Terms of Services</ion-router-link>.</p>

                </main>
            </ion-content>
        ];
    }

}
