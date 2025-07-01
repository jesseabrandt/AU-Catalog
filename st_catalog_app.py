import streamlit as st
import pandas as pd

"# AU Catalog Topic Modelling"
"Course descriptions were analyzed using Latent Dirichlet Allocation (LDA) to find 20 topics based on which words appeared together. These topics were manually classified based on top words."
"### Average Course Topic Gamma by School"
schools = pd.read_csv("by_school.csv")
#h = df.head()
#st.write(h)

st.write(schools)
topics = schools.columns.drop("school")
topic = st.selectbox(label = "Topic", options=topics)
st.bar_chart(data = schools, x = "school", y = topic)

"### Average Course Topic Gamma by Department"
depts = pd.read_csv("by_dept.csv")

st.write(depts)

dept_names = pd.unique(depts["dept"])
depts_picked = st.multiselect(options = dept_names, label = "Departments")
dept_years = pd.read_csv("by_dept_year.csv")
st.line_chart(data = dept_years[dept_years["dept"].isin (depts_picked)], x="year (fall)", y = topic, color = "dept")