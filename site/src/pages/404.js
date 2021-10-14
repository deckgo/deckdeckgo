import * as React from 'react';

import {Link} from 'gatsby';

import Layout from '../components/core/layout/en';

import {Footer} from '../components/core/footer/footer';

import {main} from '../themes/templates/template.module.scss';

const Index404Page = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <div className={main}>
          <h1>Not found</h1>
          <p>
            <Link to="/">Go home</Link>.
          </p>
        </div>
      </section>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default Index404Page;
