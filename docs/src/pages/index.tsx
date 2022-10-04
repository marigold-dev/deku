import React from 'react';
// import clsx from 'clsx';
// import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
// import HomepageFeatures from '@site/src/components/HomepageFeatures';

// import styles from './index.module.css';

export default function Home(): JSX.Element {
  const {siteConfig} = useDocusaurusContext();
  // TODO: design a pretty landing page
  window.location.href = "/docs/intro";
  return (
    <Layout
      title={`Hello from ${siteConfig.title}`}
      description="TODO: add a description">
      <main>
      </main>
    </Layout>
  );
}
