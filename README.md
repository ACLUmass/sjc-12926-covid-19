# SJC 12926 - COVID-19 in Prisons

Web application to track testing, positive cases, and releases in prisons and jails across Massachusetts during the COVID-19 pandemic

Production site: https://data.aclum.org/sjc-12926-tracker/

## Deployment workflow
<table width="300">
  <tr>
    <th> Local </th>
    <th> Github </th>
    <th> Virtual Private Server </th>
  </tr>
  <tr>
    <td> R Shiny app code updates are pushed to the git repo... </td>
    <td> ...which, if pushed to the <code>main</code> branch, triggers an <code>rsync</code> Action...</td>
    <td> ...which copies the updated app code to our virtual private server. Shiny server (also hosted on VPS) exposes updated app. </td>
  </tr>
</table>
