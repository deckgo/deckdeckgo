import React, {createRef, useEffect, useRef, useState} from 'react';

import {FormattedMessage, useIntl} from 'react-intl';

import {graphql, Link, StaticQuery} from 'gatsby';
import {GatsbyImage} from 'gatsby-plugin-image';

import {header, dark as darkStyle, end, fix as fixStyle, home, menu, nav, start, tertiary} from './navigation.module.scss';

import {LinkButton} from '../buttons/link-button';
import {Menu} from '../menu/menu';
import {Announcement} from '../announcement/announcement';

const Navigation = ({data, fix, lang, navTheme}) => {
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

  const menuRef = createRef();

  const openMenu = () => {
    menuRef.current.open();
  };

  const theme = navTheme === 'enterprise' ? darkStyle : navTheme === 'features' || navTheme === 'pricing' ? tertiary : '';

  return (
    <>
      <header className={`${header} ` + (fix ? `${fixStyle}` : scrolled ? `${fixStyle} animated` : '') + `${theme}`}>
        <nav className={nav}>
          <button className={menu} aria-label={intl.formatMessage({id: 'nav.menu'})} onClick={() => openMenu()}>
            <img
              loading="lazy"
              src={`/assets/icons/ionicons/menu${navTheme ? '-dark' : ''}.svg`}
              aria-hidden="true"
              alt=""
              style={{width: '2rem'}}
            />
          </button>

          <div className={start}>
            {renderDeckDeckGo()}

          </div>

        </nav>
      </header>

      <Menu ref={menuRef} lang={lang} />
    </>
  );

  function renderDeckDeckGo() {
    return (
      <Link to={`/${lang}/`} className={home}>
        <GatsbyImage image={data.placeholderImage.childImageSharp.gatsbyImageData} alt="" />

        <span style={{margin: 0}}>{data.site.siteMetadata.title}</span>
      </Link>
    );
  }
};

const LayoutQuery = (props) => (
  <StaticQuery
    query={graphql`
      query {
        placeholderImage: file(relativePath: {eq: "icon-192x192.png"}) {
          childImageSharp {
            gatsbyImageData(width: 48, placeholder: BLURRED)
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

export default LayoutQuery;
