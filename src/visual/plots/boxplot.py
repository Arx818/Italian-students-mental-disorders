import matplotlib.pyplot as plt
import matplotlib.cm as cm
from common.report import Report
from matplotlib.lines import Line2D

def init(ax, fig, color):
    """Define limits, grid and colorbar"""
    ax.set(xlim=(0, 32),
           ylim=(-1, 65))
    plt.grid (axis = 'y', color = 'black', linestyle = '--', linewidth = 0.5)
    plt.axvline(x = 16, color = 'black', label = 'axvline - full height')
    cbar = fig.colorbar(cm.ScalarMappable(cmap=color))
    cbar.set_ticks([0, 1])
    cbar.set_ticklabels(['Median\nLow\nValue', 'Median\nHigh\nValue'])

def bdi_plot_males(df_before, df_during, df_post):
    """bdi boxplot males"""
    fig, ax = plt.subplots()
    init(ax, fig, 'Blues')

    # Disorder Males/Females -- BEFORE
    df_dis_before = Report(df_before.show())
    df_dis_before.disorder(True)
    df_dis_before.phase("Before")    
    df_dis_males_before = Report(df_dis_before.show())
    df_dis_males_before.males()

    # No Disorder Males/Females -- BEFORE
    df_nodis_before = Report(df_before.show())
    df_nodis_before.disorder(False)
    df_nodis_before.phase("Before")
    df_nodis_males_before = Report(df_nodis_before.show())
    df_nodis_males_before.males()

    # Disorder Males/Females -- DURING
    df_dis_during = Report(df_during.show())
    df_dis_during.disorder(True)
    df_dis_during.phase("During")
    df_dis_males_during = Report(df_dis_during.show())
    df_dis_males_during.males()

    # No Disorder Males/Females -- DURING
    df_nodis_during = Report(df_during.show())
    df_nodis_during.disorder(False)
    df_nodis_during.phase("During")
    df_nodis_males_during = Report(df_nodis_during.show())
    df_nodis_males_during.males()

    # Disorder Males/Females -- POST
    df_dis_post = Report(df_post.show())
    df_dis_post.disorder(True)
    df_dis_post.phase("After")
    df_dis_males_post = Report(df_dis_post.show())
    df_dis_males_post.males()

    # No Disorder Males/Females -- POST
    df_nodis_post = Report(df_post.show())
    df_nodis_post.disorder(False)
    df_nodis_post.phase("After")
    df_nodis_males_post = Report(df_nodis_post.show())
    df_nodis_males_post.males()

    meanpointprops = dict(marker='d', markeredgecolor='black',
                      markerfacecolor='white')

    box_pre = ax.boxplot(df_dis_males_before.bdi_sum(), patch_artist=True, positions=[2], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_dur = ax.boxplot(df_dis_males_during.bdi_sum(), patch_artist=True, positions=[6], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_post = ax.boxplot(df_dis_males_post.bdi_sum(), patch_artist=True, positions=[10], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    
    box_pre_dis = ax.boxplot(df_nodis_males_before.bdi_sum(), patch_artist=True, positions=[22],meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_dur_dis = ax.boxplot(df_nodis_males_during.bdi_sum(), patch_artist=True, positions=[26], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_post_dis = ax.boxplot(df_nodis_males_post.bdi_sum(), patch_artist=True, positions=[30], meanprops=meanpointprops,
        widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})

    median=Line2D([0], [0], color = '#000000', lw=3)
    box=[box_pre, box_dur, box_post, box_pre_dis, box_dur_dis, box_post_dis]

    for b in box:
        for median in b['medians']:
            median.set_color('black')


    plt.title("BDI boxplots males")
    plt.ylabel("BDI score")
    plt.xlabel("Periods of Covid-19 lockdown")
    ax2 = ax.twiny()
    ax2.set_xticks([])
    ax2.set_xlabel('Disorder                                                    No Disorder', color='b')
    ax.set_xticklabels(labels=["Pre", "During", "Post", "Pre", "During", "post"])
    #0648FF, 668DFA, A1BAFF, B9CCFF
    box_pre['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#0648FF')
    box_dur['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#A1BAFF')
    box_post['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#0648FF')
    box_pre_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_dur_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_post_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')

    plt.savefig("./assets/boxplot/bdi_males-plot.png", dpi=180)

def bdi_plot_females(df_before, df_during, df_post):
    """bdi boxplot females"""
    fig, ax = plt.subplots()
    init(ax, fig, 'Reds')

    # Disorder Males/Females -- BEFORE
    df_dis_before = Report(df_before.show())
    df_dis_before.disorder(True)
    df_dis_before.phase("Before")
    df_dis_females_before = Report(df_dis_before.show())
    df_dis_females_before.females()

    # No Disorder Males/Females -- BEFORE
    df_nodis_before = Report(df_before.show())
    df_nodis_before.disorder(False)
    df_nodis_before.phase("Before")
    df_nodis_females_before = Report(df_nodis_before.show())
    df_nodis_females_before.females()

    # Disorder Females/Fefemales -- DURING
    df_dis_during = Report(df_during.show())
    df_dis_during.disorder(True)
    df_dis_during.phase("During")
    df_dis_females_during = Report(df_dis_during.show())
    df_dis_females_during.females()

    # No Disorder Females/Fefemales -- DURING
    df_nodis_during = Report(df_during.show())
    df_nodis_during.disorder(False)
    df_nodis_during.phase("During")
    df_nodis_females_during = Report(df_nodis_during.show())
    df_nodis_females_during.females()

    # Disorder Females/Fefemales -- POST
    df_dis_post = Report(df_post.show())
    df_dis_post.disorder(True)
    df_dis_post.phase("After")
    df_dis_females_post = Report(df_dis_post.show())
    df_dis_females_post.females()

    # No Disorder Females/Fefemales -- POST
    df_nodis_post = Report(df_post.show())
    df_nodis_post.disorder(False)
    df_nodis_post.phase("After")
    df_nodis_females_post = Report(df_nodis_post.show())
    df_nodis_females_post.females()

    meanpointprops = dict(marker='d', markeredgecolor='black',
                      markerfacecolor='white')

    box_pre = ax.boxplot(df_dis_females_before.bdi_sum(), patch_artist=True, positions=[2], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_dur = ax.boxplot(df_dis_females_during.bdi_sum(), patch_artist=True, positions=[6], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_post = ax.boxplot(df_dis_females_post.bdi_sum(), patch_artist=True, positions=[10], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    
    box_pre_dis = ax.boxplot(df_nodis_females_before.bdi_sum(), patch_artist=True, positions=[22],meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_dur_dis = ax.boxplot(df_nodis_females_during.bdi_sum(), patch_artist=True, positions=[26], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_post_dis = ax.boxplot(df_nodis_females_post.bdi_sum(), patch_artist=True, positions=[30], meanprops=meanpointprops,
        widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})

    median=Line2D([0], [0], color = '#000000', lw=3)
    box=[box_pre, box_dur, box_post, box_pre_dis, box_dur_dis, box_post_dis]

    for b in box:
        for median in b['medians']:
            median.set_color('black')


    plt.title("BDI boxplots females")
    plt.ylabel("BDI score")
    plt.xlabel("Periods of Covid-19 lockdown")
    ax2 = ax.twiny()
    ax2.set_xticks([])
    ax2.set_xlabel('Disorder                                                    No Disorder', color='r')
    ax.set_xticklabels(labels=["Pre", "During", "Post", "Pre", "During", "post"])
    #DE0909, EB5151, F08383 					
    box_pre['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_dur['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_post['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#DE0909')
    box_pre_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#F08383')
    box_dur_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#F08383')
    box_post_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')

    plt.savefig("./assets/boxplot/bdi_females-plot.png", dpi=180)

def bai_plot_males(df_before, df_during, df_post):
    """bai boxplot males"""
    fig, ax = plt.subplots()
    init(ax, fig, 'Blues')

    # Disorder Males/Females -- BEFORE
    df_dis_before = Report(df_before.show())
    df_dis_before.disorder(True)
    df_dis_before.phase("Before")    
    df_dis_males_before = Report(df_dis_before.show())
    df_dis_males_before.males()

    # No Disorder Males/Females -- BEFORE
    df_nodis_before = Report(df_before.show())
    df_nodis_before.disorder(False)
    df_nodis_before.phase("Before")
    df_nodis_males_before = Report(df_nodis_before.show())
    df_nodis_males_before.males()

    # Disorder Males/Females -- DURING
    df_dis_during = Report(df_during.show())
    df_dis_during.disorder(True)
    df_dis_during.phase("During")
    df_dis_males_during = Report(df_dis_during.show())
    df_dis_males_during.males()

    # No Disorder Males/Females -- DURING
    df_nodis_during = Report(df_during.show())
    df_nodis_during.disorder(False)
    df_nodis_during.phase("During")
    df_nodis_males_during = Report(df_nodis_during.show())
    df_nodis_males_during.males()

    # Disorder Males/Females -- POST
    df_dis_post = Report(df_post.show())
    df_dis_post.disorder(True)
    df_dis_post.phase("After")
    df_dis_males_post = Report(df_dis_post.show())
    df_dis_males_post.males()

    # No Disorder Males/Females -- POST
    df_nodis_post = Report(df_post.show())
    df_nodis_post.disorder(False)
    df_nodis_post.phase("After")
    df_nodis_males_post = Report(df_nodis_post.show())
    df_nodis_males_post.males()

    meanpointprops = dict(marker='d', markeredgecolor='black',
                      markerfacecolor='white')

    box_pre = ax.boxplot(df_dis_males_before.bai_sum(), patch_artist=True, positions=[2], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_dur = ax.boxplot(df_dis_males_during.bai_sum(), patch_artist=True, positions=[6], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_post = ax.boxplot(df_dis_males_post.bai_sum(), patch_artist=True, positions=[10], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    
    box_pre_dis = ax.boxplot(df_nodis_males_before.bai_sum(), patch_artist=True, positions=[22],meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_dur_dis = ax.boxplot(df_nodis_males_during.bai_sum(), patch_artist=True, positions=[26], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_post_dis = ax.boxplot(df_nodis_males_post.bai_sum(), patch_artist=True, positions=[30], meanprops=meanpointprops,
        widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})

    median=Line2D([0], [0], color = '#000000', lw=3)
    box=[box_pre, box_dur, box_post, box_pre_dis, box_dur_dis, box_post_dis]

    for b in box:
        for median in b['medians']:
            median.set_color('black')


    plt.title("BAI boxplots males")
    plt.ylabel("BAI score")
    plt.xlabel("Periods of Covid-19 lockdown")
    ax2 = ax.twiny()
    ax2.set_xticks([])
    ax2.set_xlabel('Disorder                                                    No Disorder', color='b')
    ax.set_xticklabels(labels=["Pre", "During", "Post", "Pre", "During", "post"])
    #0648FF, 668DFA, A1BAFF, B9CCFF
    box_pre['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#0648FF')
    box_dur['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_post['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_pre_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#A1BAFF')
    box_dur_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#B9CCFF')
    box_post_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#A1BAFF')

    plt.savefig("./assets/boxplot/bai_males-plot.png", dpi=180)

def bai_plot_females(df_before, df_during, df_post):
    """bai boxplot females"""
    fig, ax = plt.subplots()
    init(ax, fig, 'Reds')

    # Disorder Males/Females -- BEFORE
    df_dis_before = Report(df_before.show())
    df_dis_before.disorder(True)
    df_dis_before.phase("Before")
    df_dis_females_before = Report(df_dis_before.show())
    df_dis_females_before.females()

    # No Disorder Males/Females -- BEFORE
    df_nodis_before = Report(df_before.show())
    df_nodis_before.disorder(False)
    df_nodis_before.phase("Before")
    df_nodis_females_before = Report(df_nodis_before.show())
    df_nodis_females_before.females()

    # Disorder Females/Fefemales -- DURING
    df_dis_during = Report(df_during.show())
    df_dis_during.disorder(True)
    df_dis_during.phase("During")
    df_dis_females_during = Report(df_dis_during.show())
    df_dis_females_during.females()

    # No Disorder Females/Fefemales -- DURING
    df_nodis_during = Report(df_during.show())
    df_nodis_during.disorder(False)
    df_nodis_during.phase("During")
    df_nodis_females_during = Report(df_nodis_during.show())
    df_nodis_females_during.females()

    # Disorder Females/Fefemales -- POST
    df_dis_post = Report(df_post.show())
    df_dis_post.disorder(True)
    df_dis_post.phase("After")
    df_dis_females_post = Report(df_dis_post.show())
    df_dis_females_post.females()

    # No Disorder Females/Fefemales -- POST
    df_nodis_post = Report(df_post.show())
    df_nodis_post.disorder(False)
    df_nodis_post.phase("After")
    df_nodis_females_post = Report(df_nodis_post.show())
    df_nodis_females_post.females()

    meanpointprops = dict(marker='d', markeredgecolor='black',
                      markerfacecolor='white')

    box_pre = ax.boxplot(df_dis_females_before.bai_sum(), patch_artist=True, positions=[2], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_dur = ax.boxplot(df_dis_females_during.bai_sum(), patch_artist=True, positions=[6], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_post = ax.boxplot(df_dis_females_post.bai_sum(), patch_artist=True, positions=[10], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    
    box_pre_dis = ax.boxplot(df_nodis_females_before.bai_sum(), patch_artist=True, positions=[22],meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_dur_dis = ax.boxplot(df_nodis_females_during.bai_sum(), patch_artist=True, positions=[26], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_post_dis = ax.boxplot(df_nodis_females_post.bai_sum(), patch_artist=True, positions=[30], meanprops=meanpointprops,
        widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})

    median=Line2D([0], [0], color = '#000000', lw=3)
    box=[box_pre, box_dur, box_post, box_pre_dis, box_dur_dis, box_post_dis]

    for b in box:
        for median in b['medians']:
            median.set_color('black')


    plt.title("BAI boxplots females")
    plt.ylabel("BAI score")
    plt.xlabel("Periods of Covid-19 lockdown")
    ax2 = ax.twiny()
    ax2.set_xticks([])
    ax2.set_xlabel('Disorder                                                    No Disorder', color='r')
    ax.set_xticklabels(labels=["Pre", "During", "Post", "Pre", "During", "post"])
    #DE0909, EB5151, F08383 
    box_pre['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_dur['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_post['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#DE0909')
    box_pre_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_dur_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#F08383')
    box_post_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#F08383')

    plt.savefig("./assets/boxplot/bai_females-plot.png", dpi=180)

def ocir_plot_males(df_before, df_during, df_post):
    """oci boxplot males"""
    fig, ax = plt.subplots()
    init(ax, fig, 'Blues')

    # Disorder Males/Females -- BEFORE
    df_dis_before = Report(df_before.show())
    df_dis_before.disorder(True)
    df_dis_before.phase("Before")    
    df_dis_males_before = Report(df_dis_before.show())
    df_dis_males_before.males()

    # No Disorder Males/Females -- BEFORE
    df_nodis_before = Report(df_before.show())
    df_nodis_before.disorder(False)
    df_nodis_before.phase("Before")
    df_nodis_males_before = Report(df_nodis_before.show())
    df_nodis_males_before.males()

    # Disorder Males/Females -- DURING
    df_dis_during = Report(df_during.show())
    df_dis_during.disorder(True)
    df_dis_during.phase("During")
    df_dis_males_during = Report(df_dis_during.show())
    df_dis_males_during.males()

    # No Disorder Males/Females -- DURING
    df_nodis_during = Report(df_during.show())
    df_nodis_during.disorder(False)
    df_nodis_during.phase("During")
    df_nodis_males_during = Report(df_nodis_during.show())
    df_nodis_males_during.males()

    # Disorder Males/Females -- POST
    df_dis_post = Report(df_post.show())
    df_dis_post.disorder(True)
    df_dis_post.phase("After")
    df_dis_males_post = Report(df_dis_post.show())
    df_dis_males_post.males()

    # No Disorder Males/Females -- POST
    df_nodis_post = Report(df_post.show())
    df_nodis_post.disorder(False)
    df_nodis_post.phase("After")
    df_nodis_males_post = Report(df_nodis_post.show())
    df_nodis_males_post.males()

    meanpointprops = dict(marker='d', markeredgecolor='black',
                      markerfacecolor='white')

    box_pre = ax.boxplot(df_dis_males_before.oci_sum(), patch_artist=True, positions=[2], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_dur = ax.boxplot(df_dis_males_during.oci_sum(), patch_artist=True, positions=[6], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_post = ax.boxplot(df_dis_males_post.oci_sum(), patch_artist=True, positions=[10], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    
    box_pre_dis = ax.boxplot(df_nodis_males_before.oci_sum(), patch_artist=True, positions=[22],meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_dur_dis = ax.boxplot(df_nodis_males_during.oci_sum(), patch_artist=True, positions=[26], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_post_dis = ax.boxplot(df_nodis_males_post.oci_sum(), patch_artist=True, positions=[30], meanprops=meanpointprops,
        widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})

    median=Line2D([0], [0], color = '#000000', lw=3)
    box=[box_pre, box_dur, box_post, box_pre_dis, box_dur_dis, box_post_dis]

    for b in box:
        for median in b['medians']:
            median.set_color('black')


    plt.title("OCI-R boxplots males")
    plt.ylabel("OCI-R score")
    plt.xlabel("Periods of Covid-19 lockdown")
    ax2 = ax.twiny()
    ax2.set_xticks([])
    ax2.set_xlabel('Disorder                                                    No Disorder', color='b')
    ax.set_xticklabels(labels=["Pre", "During", "Post", "Pre", "During", "post"])
    #0648FF, 668DFA, A1BAFF, B9CCFF
    box_pre['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#0648FF')
    box_dur['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_post['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#A1BAFF')
    box_pre_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#A1BAFF')
    box_dur_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#B9CCFF')
    box_post_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')

    plt.savefig("./assets/boxplot/ocir_males-plot.png", dpi=180)

def ocir_plot_females(df_before, df_during, df_post):
    """oci boxplot females"""
    fig, ax = plt.subplots()
    init(ax, fig, 'Reds')

    # Disorder Males/Females -- BEFORE
    df_dis_before = Report(df_before.show())
    df_dis_before.disorder(True)
    df_dis_before.phase("Before")
    df_dis_females_before = Report(df_dis_before.show())
    df_dis_females_before.females()

    # No Disorder Males/Females -- BEFORE
    df_nodis_before = Report(df_before.show())
    df_nodis_before.disorder(False)
    df_nodis_before.phase("Before")
    df_nodis_females_before = Report(df_nodis_before.show())
    df_nodis_females_before.females()

    # Disorder Females/Fefemales -- DURING
    df_dis_during = Report(df_during.show())
    df_dis_during.disorder(True)
    df_dis_during.phase("During")
    df_dis_females_during = Report(df_dis_during.show())
    df_dis_females_during.females()

    # No Disorder Females/Fefemales -- DURING
    df_nodis_during = Report(df_during.show())
    df_nodis_during.disorder(False)
    df_nodis_during.phase("During")
    df_nodis_females_during = Report(df_nodis_during.show())
    df_nodis_females_during.females()

    # Disorder Females/Fefemales -- POST
    df_dis_post = Report(df_post.show())
    df_dis_post.disorder(True)
    df_dis_post.phase("After")
    df_dis_females_post = Report(df_dis_post.show())
    df_dis_females_post.females()

    # No Disorder Females/Fefemales -- POST
    df_nodis_post = Report(df_post.show())
    df_nodis_post.disorder(False)
    df_nodis_post.phase("After")
    df_nodis_females_post = Report(df_nodis_post.show())
    df_nodis_females_post.females()

    meanpointprops = dict(marker='d', markeredgecolor='black',
                      markerfacecolor='white')

    box_pre = ax.boxplot(df_dis_females_before.oci_sum(), patch_artist=True, positions=[2], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_dur = ax.boxplot(df_dis_females_during.oci_sum(), patch_artist=True, positions=[6], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_post = ax.boxplot(df_dis_females_post.oci_sum(), patch_artist=True, positions=[10], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    
    box_pre_dis = ax.boxplot(df_nodis_females_before.oci_sum(), patch_artist=True, positions=[22],meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_dur_dis = ax.boxplot(df_nodis_females_during.oci_sum(), patch_artist=True, positions=[26], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_post_dis = ax.boxplot(df_nodis_females_post.oci_sum(), patch_artist=True, positions=[30], meanprops=meanpointprops,
        widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})

    median=Line2D([0], [0], color = '#000000', lw=3)
    box=[box_pre, box_dur, box_post, box_pre_dis, box_dur_dis, box_post_dis]

    for b in box:
        for median in b['medians']:
            median.set_color('black')


    plt.title("OCI-R boxplots females")
    plt.ylabel("OCI-R score")
    plt.xlabel("Periods of Covid-19 lockdown")
    ax2 = ax.twiny()
    ax2.set_xticks([])
    ax2.set_xlabel('Disorder                                                    No Disorder', color='r')
    ax.set_xticklabels(labels=["Pre", "During", "Post", "Pre", "During", "post"])
    #DE0909, EB5151, F08383 
    box_pre['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_dur['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#DE0909')
    box_post['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#F08383')
    box_pre_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#F08383')
    box_dur_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#F08383')
    box_post_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#F08383')

    plt.savefig("./assets/boxplot/ocir_females-plot.png", dpi=180)

def ehq_plot_males(df_before, df_during, df_post):
    """ehq boxplot males"""
    fig, ax = plt.subplots()
    init(ax, fig, 'Blues')

    ax.set(xlim=(0, 32),
           ylim=(20, 80))
    

    # Disorder Males/Females -- BEFORE
    df_dis_before = Report(df_before.show())
    df_dis_before.disorder(True)
    df_dis_before.phase("Before")    
    df_dis_males_before = Report(df_dis_before.show())
    df_dis_males_before.males()

    # No Disorder Males/Females -- BEFORE
    df_nodis_before = Report(df_before.show())
    df_nodis_before.disorder(False)
    df_nodis_before.phase("Before")
    df_nodis_males_before = Report(df_nodis_before.show())
    df_nodis_males_before.males()

    # Disorder Males/Females -- DURING
    df_dis_during = Report(df_during.show())
    df_dis_during.disorder(True)
    df_dis_during.phase("During")
    df_dis_males_during = Report(df_dis_during.show())
    df_dis_males_during.males()

    # No Disorder Males/Females -- DURING
    df_nodis_during = Report(df_during.show())
    df_nodis_during.disorder(False)
    df_nodis_during.phase("During")
    df_nodis_males_during = Report(df_nodis_during.show())
    df_nodis_males_during.males()

    # Disorder Males/Females -- POST
    df_dis_post = Report(df_post.show())
    df_dis_post.disorder(True)
    df_dis_post.phase("After")
    df_dis_males_post = Report(df_dis_post.show())
    df_dis_males_post.males()

    # No Disorder Males/Females -- POST
    df_nodis_post = Report(df_post.show())
    df_nodis_post.disorder(False)
    df_nodis_post.phase("After")
    df_nodis_males_post = Report(df_nodis_post.show())
    df_nodis_males_post.males()

    meanpointprops = dict(marker='d', markeredgecolor='black',
                      markerfacecolor='white')

    box_pre = ax.boxplot(df_dis_males_before.ehq_sum(), patch_artist=True, positions=[2], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_dur = ax.boxplot(df_dis_males_during.ehq_sum(), patch_artist=True, positions=[6], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_post = ax.boxplot(df_dis_males_post.ehq_sum(), patch_artist=True, positions=[10], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    
    box_pre_dis = ax.boxplot(df_nodis_males_before.ehq_sum(), patch_artist=True, positions=[22],meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_dur_dis = ax.boxplot(df_nodis_males_during.ehq_sum(), patch_artist=True, positions=[26], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_post_dis = ax.boxplot(df_nodis_males_post.ehq_sum(), patch_artist=True, positions=[30], meanprops=meanpointprops,
        widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})

    median=Line2D([0], [0], color = '#000000', lw=3)
    box=[box_pre, box_dur, box_post, box_pre_dis, box_dur_dis, box_post_dis]

    for b in box:
        for median in b['medians']:
            median.set_color('black')


    plt.title("EHQ boxplots males")
    plt.ylabel("EHQ score")
    plt.xlabel("Periods of Covid-19 lockdown")
    ax2 = ax.twiny()
    ax2.set_xticks([])
    ax2.set_xlabel('Disorder                                                    No Disorder', color='b')
    ax.set_xticklabels(labels=["Pre", "During", "Post", "Pre", "During", "post"])
    #0648FF, 668DFA, A1BAFF
    box_pre['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#A1BAFF')
    box_dur['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#0648FF')
    box_post['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_pre_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_dur_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#0648FF')
    box_post_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')

    plt.savefig("./assets/boxplot/ehq_males-plot.png", dpi=180)

def ehq_plot_females(df_before, df_during, df_post):
    """ehq boxplot females"""
    fig, ax = plt.subplots()
    init(ax, fig, 'Reds')

    ax.set(xlim=(0, 32),
           ylim=(20, 80))
    

    # Disorder Males/Females -- BEFORE
    df_dis_before = Report(df_before.show())
    df_dis_before.disorder(True)
    df_dis_before.phase("Before")
    df_dis_females_before = Report(df_dis_before.show())
    df_dis_females_before.females()

    # No Disorder Males/Females -- BEFORE
    df_nodis_before = Report(df_before.show())
    df_nodis_before.disorder(False)
    df_nodis_before.phase("Before")
    df_nodis_females_before = Report(df_nodis_before.show())
    df_nodis_females_before.females()

    # Disorder Females/Fefemales -- DURING
    df_dis_during = Report(df_during.show())
    df_dis_during.disorder(True)
    df_dis_during.phase("During")
    df_dis_females_during = Report(df_dis_during.show())
    df_dis_females_during.females()

    # No Disorder Females/Fefemales -- DURING
    df_nodis_during = Report(df_during.show())
    df_nodis_during.disorder(False)
    df_nodis_during.phase("During")
    df_nodis_females_during = Report(df_nodis_during.show())
    df_nodis_females_during.females()

    # Disorder Females/Fefemales -- POST
    df_dis_post = Report(df_post.show())
    df_dis_post.disorder(True)
    df_dis_post.phase("After")
    df_dis_females_post = Report(df_dis_post.show())
    df_dis_females_post.females()

    # No Disorder Females/Fefemales -- POST
    df_nodis_post = Report(df_post.show())
    df_nodis_post.disorder(False)
    df_nodis_post.phase("After")
    df_nodis_females_post = Report(df_nodis_post.show())
    df_nodis_females_post.females()

    meanpointprops = dict(marker='d', markeredgecolor='black',
                      markerfacecolor='white')

    box_pre = ax.boxplot(df_dis_females_before.ehq_sum(), patch_artist=True, positions=[2], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_dur = ax.boxplot(df_dis_females_during.ehq_sum(), patch_artist=True, positions=[6], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_post = ax.boxplot(df_dis_females_post.ehq_sum(), patch_artist=True, positions=[10], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    
    box_pre_dis = ax.boxplot(df_nodis_females_before.ehq_sum(), patch_artist=True, positions=[22],meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_dur_dis = ax.boxplot(df_nodis_females_during.ehq_sum(), patch_artist=True, positions=[26], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_post_dis = ax.boxplot(df_nodis_females_post.ehq_sum(), patch_artist=True, positions=[30], meanprops=meanpointprops,
        widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})

    median=Line2D([0], [0], color = '#000000', lw=3)
    box=[box_pre, box_dur, box_post, box_pre_dis, box_dur_dis, box_post_dis]

    for b in box:
        for median in b['medians']:
            median.set_color('black')


    plt.title("EHQ boxplots females")
    plt.ylabel("EHQ score")
    plt.xlabel("Periods of Covid-19 lockdown")
    ax2 = ax.twiny()
    ax2.set_xticks([])
    ax2.set_xlabel('Disorder                                                    No Disorder', color='r')
    ax.set_xticklabels(labels=["Pre", "During", "Post", "Pre", "During", "post"])
    #DE0909, EB5151, F08383  
    box_pre['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#DE0909')
    box_dur['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#DE0909')
    box_post['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_pre_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#F08383')
    box_dur_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#F08383')
    box_post_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')

    plt.savefig("./assets/boxplot/ehq_females-plot.png", dpi=180)

def ed_plot_males(df_before, df_during, df_post):
    """ed boxplot males"""
    fig, ax = plt.subplots()
    init(ax, fig, 'Blues')

    ax.set(xlim=(0, 32),
           ylim=(-100, 100))
    
    # Disorder Males/Females -- BEFORE
    df_dis_before = Report(df_before.show())
    df_dis_before.disorder(True)
    df_dis_before.phase("Before")    
    df_dis_males_before = Report(df_dis_before.show())
    df_dis_males_before.males()

    # No Disorder Males/Females -- BEFORE
    df_nodis_before = Report(df_before.show())
    df_nodis_before.disorder(False)
    df_nodis_before.phase("Before")
    df_nodis_males_before = Report(df_nodis_before.show())
    df_nodis_males_before.males()

    # Disorder Males/Females -- DURING
    df_dis_during = Report(df_during.show())
    df_dis_during.disorder(True)
    df_dis_during.phase("During")
    df_dis_males_during = Report(df_dis_during.show())
    df_dis_males_during.males()

    # No Disorder Males/Females -- DURING
    df_nodis_during = Report(df_during.show())
    df_nodis_during.disorder(False)
    df_nodis_during.phase("During")
    df_nodis_males_during = Report(df_nodis_during.show())
    df_nodis_males_during.males()

    # Disorder Males/Females -- POST
    df_dis_post = Report(df_post.show())
    df_dis_post.disorder(True)
    df_dis_post.phase("After")
    df_dis_males_post = Report(df_dis_post.show())
    df_dis_males_post.males()

    # No Disorder Males/Females -- POST
    df_nodis_post = Report(df_post.show())
    df_nodis_post.disorder(False)
    df_nodis_post.phase("After")
    df_nodis_males_post = Report(df_nodis_post.show())
    df_nodis_males_post.males()

    meanpointprops = dict(marker='d', markeredgecolor='black',
                      markerfacecolor='white')

    box_pre = ax.boxplot(df_dis_males_before.ed_sum(), patch_artist=True, positions=[2], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_dur = ax.boxplot(df_dis_males_during.ed_sum(), patch_artist=True, positions=[6], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_post = ax.boxplot(df_dis_males_post.ed_sum(), patch_artist=True, positions=[10], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    
    box_pre_dis = ax.boxplot(df_nodis_males_before.ed_sum(), patch_artist=True, positions=[22],meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_dur_dis = ax.boxplot(df_nodis_males_during.ed_sum(), patch_artist=True, positions=[26], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})
    box_post_dis = ax.boxplot(df_nodis_males_post.ed_sum(), patch_artist=True, positions=[30], meanprops=meanpointprops,
        widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'blue'})

    median=Line2D([0], [0], color = '#000000', lw=3)
    box=[box_pre, box_dur, box_post, box_pre_dis, box_dur_dis, box_post_dis]

    for b in box:
        for median in b['medians']:
            median.set_color('black')


    plt.title("ED Risk boxplots males")
    plt.ylabel("ED Risk score")
    plt.xlabel("Periods of Covid-19 lockdown")
    ax2 = ax.twiny()
    ax2.set_xticks([])
    ax2.set_xlabel('Disorder                                                    No Disorder', color='b')
    ax.set_xticklabels(labels=["Pre", "During", "Post", "Pre", "During", "post"])
    #0648FF, 668DFA, A1BAFF
    box_pre['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_dur['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_post['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_pre_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_dur_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')
    box_post_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#668DFA')

    plt.savefig("./assets/boxplot/ed_males-plot.png", dpi=180)

def ed_plot_females(df_before, df_during, df_post):
    """ed boxplot females"""
    fig, ax = plt.subplots()
    init(ax, fig, 'Reds')

    ax.set(xlim=(0, 32),
           ylim=(-100, 100))
    
    # Disorder Males/Females -- BEFORE
    df_dis_before = Report(df_before.show())
    df_dis_before.disorder(True)
    df_dis_before.phase("Before")
    df_dis_females_before = Report(df_dis_before.show())
    df_dis_females_before.females()

    # No Disorder Males/Females -- BEFORE
    df_nodis_before = Report(df_before.show())
    df_nodis_before.disorder(False)
    df_nodis_before.phase("Before")
    df_nodis_females_before = Report(df_nodis_before.show())
    df_nodis_females_before.females()

    # Disorder Females/Fefemales -- DURING
    df_dis_during = Report(df_during.show())
    df_dis_during.disorder(True)
    df_dis_during.phase("During")
    df_dis_females_during = Report(df_dis_during.show())
    df_dis_females_during.females()

    # No Disorder Females/Fefemales -- DURING
    df_nodis_during = Report(df_during.show())
    df_nodis_during.disorder(False)
    df_nodis_during.phase("During")
    df_nodis_females_during = Report(df_nodis_during.show())
    df_nodis_females_during.females()

    # Disorder Females/Fefemales -- POST
    df_dis_post = Report(df_post.show())
    df_dis_post.disorder(True)
    df_dis_post.phase("After")
    df_dis_females_post = Report(df_dis_post.show())
    df_dis_females_post.females()

    # No Disorder Females/Fefemales -- POST
    df_nodis_post = Report(df_post.show())
    df_nodis_post.disorder(False)
    df_nodis_post.phase("After")
    df_nodis_females_post = Report(df_nodis_post.show())
    df_nodis_females_post.females()

    meanpointprops = dict(marker='d', markeredgecolor='black',
                      markerfacecolor='white')

    box_pre = ax.boxplot(df_dis_females_before.ed_sum(), patch_artist=True, positions=[2], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_dur = ax.boxplot(df_dis_females_during.ed_sum(), patch_artist=True, positions=[6], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_post = ax.boxplot(df_dis_females_post.ed_sum(), patch_artist=True, positions=[10], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    
    box_pre_dis = ax.boxplot(df_nodis_females_before.ed_sum(), patch_artist=True, positions=[22],meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_dur_dis = ax.boxplot(df_nodis_females_during.ed_sum(), patch_artist=True, positions=[26], meanprops=meanpointprops,
            widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})
    box_post_dis = ax.boxplot(df_nodis_females_post.ed_sum(), patch_artist=True, positions=[30], meanprops=meanpointprops,
        widths=2.2, showmeans=True, flierprops={'marker': 'd', 'markersize': 6, 'markerfacecolor': 'red'})

    median=Line2D([0], [0], color = '#000000', lw=3)
    box=[box_pre, box_dur, box_post, box_pre_dis, box_dur_dis, box_post_dis]

    for b in box:
        for median in b['medians']:
            median.set_color('black')


    plt.title("ED Risk boxplots females")
    plt.ylabel("ED Risk score")
    plt.xlabel("Periods of Covid-19 lockdown")
    ax2 = ax.twiny()
    ax2.set_xticks([])
    ax2.set_xlabel('Disorder                                                    No Disorder', color='r')
    ax.set_xticklabels(labels=["Pre", "During", "Post", "Pre", "During", "post"])
    #DE0909, EB5151, F08383  
    box_pre['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_dur['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#DE0909')
    box_post['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_pre_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_dur_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')
    box_post_dis['boxes'][0].set(color='black', linewidth=0.5, facecolor = '#EB5151')

    plt.savefig("./assets/boxplot/ed_females-plot.png", dpi=180)

def box_plot(df_before, df_during, df_after):
    """functions call"""
    bdi_plot_males(df_before, df_during, df_after)
    bdi_plot_females(df_before, df_during, df_after)
    bai_plot_males(df_before, df_during, df_after)
    bai_plot_females(df_before, df_during, df_after)
    ocir_plot_males(df_before, df_during, df_after)
    ocir_plot_females(df_before, df_during, df_after)
    ehq_plot_males(df_before, df_during, df_after)
    ehq_plot_females(df_before, df_during, df_after)
    ed_plot_males(df_before, df_during, df_after)
    ed_plot_females(df_before, df_during, df_after)
