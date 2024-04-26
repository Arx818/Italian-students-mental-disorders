from common.report import Report
from plots.boxplot import box_plot
from plots.barchart import bar_chart


def run():
    """Create a dataframe and plot data from them"""
    df = Report('./data/dataset.csv')
    df2 = Report('./data/half.csv')
    df3 = Report('./data/half.csv')

    df.phase("Before")
    df2.phase("During")
    df3.phase("Post")
    box_plot(df, df2, df3)
    bar_chart(df, df2, df3)


if __name__ == '__main__':
    run()
