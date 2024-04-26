import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from common.report import Report


def init(ax, ylim):
    """create caption and set limits"""
    custom_lines = [Line2D([0], [0], color=(0.3,0.9,0.4,0.6), lw=3),
                    Line2D([0], [0], color=(0.3,0.1,0.4,0.6), lw=3)]

    ax.legend(custom_lines, ['Males', 'Females'])
    ax.set(xlim=(0, 5),
           ylim=(0, ylim))


def oci_barchart(df_before, df_during, df_post):
    """oci score means"""

    _, ax = plt.subplots()
    init(ax, 25)

    plt.title("Obsessive Compulsive Inventory (OCI-R) score means")
    plt.ylabel("Average OCI-R score")
    plt.xlabel("")

    ax.axhline(20, linestyle='--', color='k', linewidth=0.5)
    plt.annotate("Clinical threshold", (2,20))

    # Females
    df_before_fem = Report(df_before.show())
    df_before_fem.females()
    df_during_fem = Report(df_during.show())
    df_during_fem.females()
    df_post_fem = Report(df_post.show())
    df_post_fem.females()
    _, mean_pre_females, _ = df_before_fem.oci_score()
    _, mean_dur_females, _ = df_during_fem.oci_score()
    _, mean_post_females, _ = df_post_fem.oci_score()
    # Males
    df_before_males = Report(df_before.show())
    df_before_males.males()
    df_during_males = Report(df_during.show())
    df_during_males.males()
    df_post_males = Report(df_post.show())
    df_post_males.males()
    _, mean_pre_males, _ = df_before_males.oci_score()
    _, mean_dur_males, _ = df_during_males.oci_score()
    _, mean_post_males, _ = df_post_males.oci_score()

    plt.bar(0.4, round(mean_pre_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(0.7, round(mean_pre_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))
    plt.bar(2.35, round(mean_dur_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(2.65, round(mean_dur_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))
    plt.bar(4.3, round(mean_post_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(4.6, round(mean_post_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))

    ax.set_xticks([0.55, 2.5, 4.45], ['Pre', 'During', 'Post'], rotation=10)

    for bars in ax.containers:
        ax.bar_label(bars)

    plt.savefig("./assets/barchart/oci-bar.png", dpi = 180)

def bai_barchart(df_before, df_during, df_post):
    """bai score means"""

    _, ax = plt.subplots()
    init(ax, 35)

    plt.title("Beck Anxiety Inventory (BAI) score means")
    plt.ylabel("Average BAI score")
    plt.xlabel("")

    ax.axhline(7, linestyle='--', color='k', linewidth=0.5)
    ax.axhline(15, linestyle='--', color='k', linewidth=0.5)
    ax.axhline(25, linestyle='--', color='k', linewidth=0.5)
    plt.annotate("Minimal", (5,4))
    plt.annotate("Mild", (5,12))
    plt.annotate("Modera-\nte", (5,20))
    plt.annotate("Severe", (5,30))

    # Females
    df_before_fem = Report(df_before.show())
    df_before_fem.females()
    df_during_fem = Report(df_during.show())
    df_during_fem.females()
    df_post_fem = Report(df_post.show())
    df_post_fem.females()
    _, mean_pre_females, _ = df_before_fem.bai_score()
    _, mean_dur_females, _ = df_during_fem.bai_score()
    _, mean_post_females, _ = df_post_fem.bai_score()
    # Males
    df_before_males = Report(df_before.show())
    df_before_males.males()
    df_during_males = Report(df_during.show())
    df_during_males.males()
    df_post_males = Report(df_post.show())
    df_post_males.males()
    _, mean_pre_males, _ = df_before_males.bai_score()
    _, mean_dur_males, _ = df_during_males.bai_score()
    _, mean_post_males, _ = df_post_males.bai_score()

    plt.bar(0.4, round(mean_pre_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(0.7, round(mean_pre_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))
    plt.bar(2.35, round(mean_dur_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(2.65, round(mean_dur_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))
    plt.bar(4.3, round(mean_post_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(4.6, round(mean_post_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))

    ax.set_xticks([0.55, 2.5, 4.45], ['Pre', 'During', 'Post'], rotation=10)

    for bars in ax.containers:
        ax.bar_label(bars)

    plt.savefig("./assets/barchart/bai-bar.png", dpi = 180)

def bdi_barchart(df_before, df_during, df_post):
    """bdi score means"""

    _, ax = plt.subplots()
    init(ax, 35)

    plt.title("Beck Depression Inventory (BDI-2) score means")
    plt.ylabel("Average BDI-2 score")
    plt.xlabel("")

    ax.axhline(9, linestyle='--', color='k', linewidth=0.5)
    ax.axhline(18, linestyle='--', color='k', linewidth=0.5)
    ax.axhline(29, linestyle='--', color='k', linewidth=0.5)
    plt.annotate("Minimal", (5,6))
    plt.annotate("Mild", (5,15))
    plt.annotate("Modera-\nte", (5,24))
    plt.annotate("Severe", (5,31))



    # Females
    df_before_fem = Report(df_before.show())
    df_before_fem.females()
    df_during_fem = Report(df_during.show())
    df_during_fem.females()
    df_post_fem = Report(df_post.show())
    df_post_fem.females()
    _, mean_pre_females, _ = df_before_fem.bdi_score()
    _, mean_dur_females, _ = df_during_fem.bdi_score()
    _, mean_post_females, _ = df_post_fem.bdi_score()
    # Males
    df_before_males = Report(df_before.show())
    df_before_males.males()
    df_during_males = Report(df_during.show())
    df_during_males.males()
    df_post_males = Report(df_post.show())
    df_post_males.males()
    _, mean_pre_males, _ = df_before_males.bdi_score()
    _, mean_dur_males, _ = df_during_males.bdi_score()
    _, mean_post_males, _ = df_post_males.bdi_score()

    plt.bar(0.4, round(mean_pre_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(0.7, round(mean_pre_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))
    plt.bar(2.35, round(mean_dur_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(2.65, round(mean_dur_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))
    plt.bar(4.3, round(mean_post_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(4.6, round(mean_post_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))

    ax.set_xticks([0.55, 2.5, 4.45], ['Pre', 'During', 'Post'], rotation=10)

    for bars in ax.containers:
        ax.bar_label(bars)

    plt.savefig("./assets/barchart/bdi-bar.png", dpi = 180)

def ehq_barchart(df_before, df_during, df_post):
    """ehq score means"""

    _, ax = plt.subplots()
    init(ax, 55)

    plt.title("Eating Habits Questionnaire (EHQ) score means")
    plt.ylabel("Average EHQ score")
    plt.xlabel("")

    # Females
    df_before_fem = Report(df_before.show())
    df_before_fem.females()
    df_during_fem = Report(df_during.show())
    df_during_fem.females()
    df_post_fem = Report(df_post.show())
    df_post_fem.females()
    _, mean_pre_females, _ = df_before_fem.ehq_score()
    _, mean_dur_females, _ = df_during_fem.ehq_score()
    _, mean_post_females, _ = df_post_fem.ehq_score()
    # Males
    df_before_males = Report(df_before.show())
    df_before_males.males()
    df_during_males = Report(df_during.show())
    df_during_males.males()
    df_post_males = Report(df_post.show())
    df_post_males.males()
    _, mean_pre_males, _ = df_before_males.ehq_score()
    _, mean_dur_males, _ = df_during_males.ehq_score()
    _, mean_post_males, _ = df_post_males.ehq_score()

    plt.bar(0.4, round(mean_pre_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(0.7, round(mean_pre_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))
    plt.bar(2.35, round(mean_dur_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(2.65, round(mean_dur_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))
    plt.bar(4.3, round(mean_post_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(4.6, round(mean_post_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))

    ax.set_xticks([0.55, 2.5, 4.45], ['Pre', 'During', 'Post'], rotation=10)

    for bars in ax.containers:
        ax.bar_label(bars)

    plt.savefig("./assets/barchart/ehq-bar.png", dpi = 180)

def ed_barchart(df_before, df_during, df_post):
    """ed score means"""

    _, ax = plt.subplots()
    init(ax, 55)
    plt.title("Eating Disorder Risk (ED Risk) score means")
    plt.ylabel("Average ED Risk score")
    plt.xlabel("")

    # Females
    df_before_fem = Report(df_before.show())
    df_before_fem.females()
    df_during_fem = Report(df_during.show())
    df_during_fem.females()
    df_post_fem = Report(df_post.show())
    df_post_fem.females()
    _, mean_pre_females, _ = df_before_fem.ed_score()
    _, mean_dur_females, _ = df_during_fem.ed_score()
    _, mean_post_females, _ = df_post_fem.ed_score()
    # Males
    df_before_males = Report(df_before.show())
    df_before_males.males()
    df_during_males = Report(df_during.show())
    df_during_males.males()
    df_post_males = Report(df_post.show())
    df_post_males.males()
    _, mean_pre_males, _ = df_before_males.ed_score()
    _, mean_dur_males, _ = df_during_males.ed_score()
    _, mean_post_males, _ = df_post_males.ed_score()

    plt.bar(0.4, round(mean_pre_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(0.7, round(mean_pre_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))
    plt.bar(2.35, round(mean_dur_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(2.65, round(mean_dur_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))
    plt.bar(4.3, round(mean_post_males, 1), width=.30, color=(0.3,0.9,0.4,0.6))
    plt.bar(4.6, round(mean_post_females, 1), width=.30, color=(0.3,0.1,0.4,0.6))

    ax.set_xticks([0.55, 2.5, 4.45], ['Pre', 'During', 'Post'], rotation=10)

    for bars in ax.containers:
        ax.bar_label(bars)

    plt.savefig("./assets/barchart/ed-bar.png", dpi = 180)

def bar_chart(df_before, df_during, df_after):
    """a"""
    ehq_barchart(df_before, df_during, df_after)
    bdi_barchart(df_before, df_during, df_after)
    bai_barchart(df_before, df_during, df_after)
    oci_barchart(df_before, df_during, df_after)
    ed_barchart(df_before, df_during, df_after)
