import React, {useEffect, useRef, useState} from 'react';

import {FormattedMessage, useIntl} from 'react-intl';

import {graphql, Link, StaticQuery} from 'gatsby';
import Img from 'gatsby-image';

import styles from './navigation.module.scss';

import {LinkButton} from '../buttons/link-button';

const Navigation = ({data, fix, lang, dark}) => {
  const intl = useIntl();

  const [scrolled, setScrolled] = useState(false);

  const scrolledRef = useRef(scrolled);
  const setScrolledState = (data) => {
    scrolledRef.current = data;
    setScrolled(data);
  };

  useEffect(() => {
    window.addEventListener('scroll', handleScroll, {passive: true});

    return () => window.removeEventListener('scroll', handleScroll, false);

    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const handleScroll = () => {
    const scrolledSize = window.pageYOffset || document.documentElement.scrollTop || document.body.scrollTop || 0;
    const scrollTrigger = scrolledSize > 16;

    if (scrolledRef.current !== scrollTrigger) {
      setScrolledState(scrollTrigger);
    }
  };

  return (
    <>
      <header className={`${styles.header} ` + (fix ? `${styles.fix}` : scrolled ? `${styles.fix} animated` : '') + `${dark ? ` ${styles.dark}` : ''}`}>
        <nav className={styles.nav}>
          <button className={styles.menu} aria-label={intl.formatMessage({id: 'nav.menu'})}>
            <img loading="lazy" src={`/assets/icons/ionicons/menu${dark ? '-dark' : ''}.svg`} aria-hidden="true" alt="" style={{width: '2rem'}} />
          </button>

          <div className={styles.start}>
            {renderDeckDeckGo()}

            <Link to={`/${lang}/discover`}>
              <FormattedMessage id="nav.discover" />
            </Link>

            <Link to={`/${lang}/enterprise`}>
              <FormattedMessage id="nav.enterprise" />
            </Link>
          </div>

          <div className={styles.end}>
            <a href="https://app.deckdeckgo.com/sigin" rel="noopener noreferrer">
              <FormattedMessage id="nav.signin" />
            </a>

            <LinkButton targetUrl="https://app.deckdeckgo.com" msgId="nav.write.presentation" color="primary"></LinkButton>
          </div>
        </nav>
      </header>
    </>
  );

  function renderDeckDeckGo() {
    return (
      <Link to={`/${lang}/`} className={styles.home}>
        <Img fluid={data.placeholderImage.childImageSharp.fluid} />

        <h3 style={{margin: 0}}>{data.site.siteMetadata.title}</h3>
      </Link>
    );
  }
};

export default (props) => (
  <StaticQuery
    query={graphql`
      query {
        placeholderImage: file(relativePath: {eq: "icon-192x192.png"}) {
          childImageSharp {
            fluid(maxWidth: 48) {
              ...GatsbyImageSharpFluid
            }
          }
        }
        site {
          siteMetadata {
            title
          }
        }
      }
    `}
    render={(data) => <Navigation data={data} {...props} />}
  />
);
