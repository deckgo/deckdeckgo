import React, {useEffect, useRef, useState} from 'react';

import {FormattedMessage, useIntl} from 'react-intl';

import {isIPad, isMobile} from '@deckdeckgo/utils';

import {main, action, container} from './landing-deck.module.scss';
import {LinkButton} from '../../core/buttons/link-button';

export const LandingDeck = () => {
  const deck = useRef();

  const intl = useIntl();

  const [deckIsBeginning, setIsBeginning] = useState(true);
  const [deckIsEnd, setIsEnd] = useState(false);
  const [deckLoaded, setDeckLoaded] = useState(false);

  const mobile = typeof window !== 'undefined' ? isMobile() && !isIPad() : false;

  useEffect(() => {
    if (!deck || !deck.current) {
      return;
    }

    const deckRef = deck.current;

    deckRef.addEventListener('slideNextDidChange', updateDeckPosition, {passive: true});
    deckRef.addEventListener('slidePrevDidChange', updateDeckPosition, {passive: true});
    deckRef.addEventListener('deckDidLoad', markDeckLoaded, {once: true});

    return () => {
      deckRef.removeEventListener('slideNextDidChange', updateDeckPosition, false);
      deckRef.removeEventListener('slidePrevDidChange', updateDeckPosition, false);
      deckRef.removeEventListener('deckDidLoad', markDeckLoaded, false);
    };
  }, [deck]);

  const markDeckLoaded = () => {
    setDeckLoaded(true);
  };

  async function updateDeckPosition() {
    if (!deck) {
      return;
    }

    setIsBeginning(await deck?.current?.isBeginning());
    setIsEnd(await deck?.current?.isEnd());
  }

  async function prevNextSlide(next) {
    if (next) {
      await deck?.current?.slideNext(false, true);
    } else {
      await deck?.current?.slidePrev(false, true);
    }
  }

  return (
    <section>
      <div className={main}>
        <article className={`${container} ${deckLoaded ? 'loaded' : ''} ${mobile ? 'mobile' : ''}`}>{renderDeck()}</article>

        {renderSlideNavigation(deckIsBeginning || deckIsEnd ? 'light' : 'dark')}
      </div>
    </section>
  );

  function renderDeck() {
    return (
      <deckgo-deck ref={deck} embedded={true} direction-mobile="horizontal">
        <deckgo-slide-title
          style={{
            '--background': 'linear-gradient(to top right, var(--color-quinary), var(--color-tertiary))',
            '--color': 'var(--color-primary-contrast)'
          }}>
          <h2 slot="title" style={{fontSize: 'var(--font-size-h1)', lineHeight: 'var(--line-height-h1)'}}>
            <FormattedMessage id="hero.deck.slide1.title" />
          </h2>
          <div slot="content">
            <div>
              <FormattedMessage id="hero.deck.slide1.content" />
            </div>

            <LinkButton
              style={{marginTop: '1.25rem'}}
              targetUrl="https://app.deckdeckgo.com"
              msgId="hero.deck.slide1.get.started"
              color="light"></LinkButton>
          </div>

          {renderSlideBackground()}
        </deckgo-slide-title>

        <deckgo-slide-title>
          <h2 slot="title">
            <FormattedMessage id="hero.deck.slide2.title" />
          </h2>
          <div slot="content" style={{display: 'flex', justifyContent: 'center', alignItems: 'center', maxWidth: '100%'}}>
            <img data-src="/assets/img/landing/mobile-light.svg" aria-hidden="true" alt="" style={{width: '2rem', padding: '0.45rem'}} />
            <img data-src="/assets/img/landing/tablet-light.svg" aria-hidden="true" alt="" style={{width: '3rem', padding: '0.45rem'}} />
            <img data-src="/assets/img/landing/desktop-light.svg" aria-hidden="true" alt="" style={{width: '4.6rem', padding: '0.45rem'}} />
            <img data-src="/assets/img/landing/projector.svg" aria-hidden="true" alt="" style={{width: '4.6rem', padding: '0.45rem'}} />
          </div>

          {renderSlideBackground()}
        </deckgo-slide-title>

        <deckgo-slide-author
          img-src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif"
          img-alt="Predefined rich and responsive templates for a quick editing">
          <h2 slot="author" style={{marginTop: '64px'}}>
            <FormattedMessage id="hero.deck.slide3.title" />
          </h2>

          {renderSlideBackground()}
        </deckgo-slide-author>

        <deckgo-slide-title>
          <h2 slot="title">
            <FormattedMessage id="hero.deck.slide4.title" />
          </h2>
          <h3 slot="content" style={{fontWeight: '300'}}>
            <FormattedMessage id="hero.deck.slide4.content" />
          </h3>

          {renderSlideBackground(
            'https://images.unsplash.com/photo-1516476892398-bdcab4c8dab8?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjcyMTQyfQ',
            'Photo by Rodrigo Gonçalves on Unsplash'
          )}
        </deckgo-slide-title>

        <deckgo-slide-title>
          <h2 slot="title">
            <FormattedMessage id="hero.deck.slide5.title" />
          </h2>
          <div slot="content">
            <deckgo-highlight-code>
              <code slot="code">{`import React, { useState } from 'react';

function Example() {
  // Declare a new state variable, which we'll call "count"
  const [count, setCount] = useState(0);

  return (
    <div>
      <p>You clicked {count} times</p>
      <button onClick={() => setCount(count + 1)}>
        Click me
      </button>
    </div>
  );
}`}</code>
            </deckgo-highlight-code>
          </div>

          {renderSlideBackground()}
        </deckgo-slide-title>

        <deckgo-slide-split style={{'--slide-split-align': 'center'}}>
          <div slot="start">
            <h2>
              <FormattedMessage id="hero.deck.slide6.title" />
            </h2>
          </div>

          <div slot="end">
            <img
              data-src={`/assets/img/landing/illustrations/progressive-app.svg`}
              aria-hidden="true"
              alt=""
              style={{width: 'calc(var(--slide-width) / 2.5)'}}
            />
          </div>

          {renderSlideBackground()}
        </deckgo-slide-split>

        <deckgo-slide-title style={{'--color': 'white'}}>
          <h2
            slot="title"
            style={{
              background: 'rgba(var(--color-light-rgb), 1)',
              color: 'var(--color-light-contrast)',
              padding: '16px',
              borderRadius: '8px',
              boxShadow: '8px 8px 16px rgba(0, 0, 0, 0.12)'
            }}>
            <FormattedMessage id="hero.deck.slide7.title" />
          </h2>

          {renderSlideBackground(
            'https://images.unsplash.com/photo-1501386761578-eac5c94b800a?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjcyMTQyfQ',
            'Photo by Nicholas Green on Unsplash'
          )}
        </deckgo-slide-title>

        <deckgo-slide-split style={{'--slide-split-align': 'center'}}>
          <div slot="start">
            <h2>
              <FormattedMessage id="hero.deck.slide8.title" />
            </h2>
          </div>

          <div slot="end">
            <img
              data-src={`/assets/img/landing/illustrations/travel.svg`}
              aria-hidden="true"
              alt=""
              style={{width: 'calc(var(--slide-width) / 3)'}}
            />
          </div>

          {renderSlideBackground()}
        </deckgo-slide-split>

        <deckgo-slide-title style={{'--background': 'var(--color-dark)', '--color': 'var(--color-dark-contrast)'}}>
          <h2 slot="title">
            <FormattedMessage id="hero.deck.slide9.title" />
          </h2>
          <div slot="content" style={{marginBottom: '48px'}}>
            <h3 style={{fontWeight: '300'}}>
              <FormattedMessage id="hero.deck.slide9.content" />
            </h3>

            <LinkButton
              style={{marginTop: '1.25rem'}}
              targetUrl="https://app.deckdeckgo.com"
              msgId="hero.deck.slide9.start.presentation"
              color="light"></LinkButton>
          </div>

          {renderSlideBackground()}
        </deckgo-slide-title>
      </deckgo-deck>
    );
  }

  function renderSlideBackground(imgSrc, imgAlt) {
    return <div slot="background">{imgSrc && imgAlt ? <img className="background" data-src={imgSrc} alt={imgAlt}></img> : undefined}</div>;
  }

  function renderSlideNavigation(color = 'primary') {
    return (
      <>
        <button
          type="button"
          className={`${action} prev ${deckIsBeginning ? 'hide' : ''}`}
          style={{'--deck-nav-color': `var(--color-${color})`}}
          onClick={() => prevNextSlide(false)}
          tabIndex={-1}
          aria-label={intl.formatMessage({id: 'hero.deck.nav.prev'})}>
          <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512">
            <title>
              <FormattedMessage id="hero.deck.nav.prev" />
            </title>
            <path
              fill="none"
              stroke="currentColor"
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth="48"
              d="M328 112L184 256l144 144"
            />
          </svg>
        </button>

        <button
          type="button"
          className={`${action} next ${deckIsEnd ? 'hide' : ''}`}
          style={{'--deck-nav-color': `var(--color-${color})`}}
          onClick={() => prevNextSlide(true)}
          tabIndex={-1}
          aria-label={intl.formatMessage({id: 'hero.deck.nav.next'})}>
          <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512">
            <title>
              <FormattedMessage id="hero.deck.nav.next" />
            </title>
            <path
              fill="none"
              stroke="currentColor"
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth="48"
              d="M184 112l144 144-144 144"
            />
          </svg>
        </button>
      </>
    );
  }
};
