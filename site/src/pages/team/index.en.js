import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';

import {main} from '../../themes/templates/template.module.scss';
import {main as teamStylesMain, avatar, social} from './team.module.scss';

const TeamPage = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <main className={main}>
          <h1>Team</h1>

          <div className={teamStylesMain}>
            <article>
              <img
                className={avatar}
                loading="lazy"
                alt="David"
                role="presentation"
                src="https://pbs.twimg.com/profile_images/941274539979366400/bTKGkd-O_400x400.jpg"
              />

              <h2>David Dal Busco</h2>

              <div className={social}>
                <a href="https://twitter.com/daviddalbusco" rel="noopener noreferrer" aria-label="Twitter">
                  <img
                    loading="lazy"
                    src="/assets/icons/ionicons/twitter.svg"
                    aria-hidden="true"
                    alt=""
                    style={{width: '2rem', padding: '0.45rem'}}
                  />
                </a>

                <a href="https://daviddalbusco.com" rel="noopener noreferrer" aria-label="Website">
                  <img
                    loading="lazy"
                    src="/assets/icons/ionicons/globe.svg"
                    aria-hidden="true"
                    alt=""
                    style={{width: '2rem', padding: '0.45rem'}}
                  />
                </a>

                <a href="https://dev.to/daviddalbusco" rel="noopener noreferrer" aria-label="Dev.to">
                  <img loading="lazy" src="/assets/icons/dev.svg" aria-hidden="true" alt="" style={{width: '2rem', padding: '0.45rem'}} />
                </a>

                <a href="https://medium.com/@david.dalbusco" rel="noopener noreferrer" aria-label="Medium">
                  <img
                    loading="lazy"
                    src="/assets/icons/medium.svg"
                    aria-hidden="true"
                    alt=""
                    style={{width: '2rem', padding: '0.45rem'}}
                  />
                </a>

                <a href="http://github.com/peterpeterparker" rel="noopener noreferrer" aria-label="GitHub">
                  <img
                    loading="lazy"
                    src="/assets/icons/ionicons/github.svg"
                    aria-hidden="true"
                    alt=""
                    style={{width: '2rem', padding: '0.45rem'}}
                  />
                </a>
              </div>

              <p>
                David is a freelancer by day and the creator of DeckDeckGo by night. He's also the organiser of the Ionic and Indie Hackers
                Meetup Zürich. He used to play in a band called VanRonMaiden, which was probably the coolest band ever but, no one will ever
                know.
              </p>
            </article>

            <article>
              <img
                className={avatar}
                loading="lazy"
                alt="Nicolas"
                role="presentation"
                src="https://pbs.twimg.com/profile_images/588789970690658305/Ru9JiWks_400x400.png"
              />

              <h2>Nicolas Mattia</h2>

              <div className={social}>
                <a href="https://twitter.com/nasmattia" rel="noopener noreferrer" aria-label="Twitter">
                  <img
                    loading="lazy"
                    src="/assets/icons/ionicons/twitter.svg"
                    aria-hidden="true"
                    alt=""
                    style={{width: '2rem', padding: '0.45rem'}}
                  />
                </a>

                <a href="https://nmattia.com" rel="noopener noreferrer" aria-label="Website">
                  <img
                    loading="lazy"
                    src="/assets/icons/ionicons/globe.svg"
                    aria-hidden="true"
                    alt=""
                    style={{width: '2rem', padding: '0.45rem'}}
                  />
                </a>

                <a href="https://github.com/nmattia" rel="noopener noreferrer" aria-label="GitHub">
                  <img
                    loading="lazy"
                    src="/assets/icons/ionicons/github.svg"
                    aria-hidden="true"
                    alt=""
                    style={{width: '2rem', padding: '0.45rem'}}
                  />
                </a>
              </div>

              <p>
                Nicolas &hellip; has a bio as soon as he'll send me a PR{' '}
                <span role="img" aria-hidden={true}>
                  😉
                </span>
                .
              </p>
            </article>
          </div>
        </main>
      </section>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default TeamPage;
