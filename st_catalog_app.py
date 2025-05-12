import streamlit as st
import pandas as pd
from scipy.stats import f_oneway
import seaborn as sns
import matplotlib.pyplot as plt
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
