from pickle import NONE
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
from toolsos.huisstijl.colors import get_os_colors
import numpy as np

font_family_setting = 'Amsterdam Sans'
fontsize_setting = 21 #20
figsize_setting =  (11, 5.5) #(10, 5.5)
figsize_setting_bar = (9, 5.5)
#figsize_setting = (8, 5.5)

#--------------------------------------------------
# BAR PLOT
#--------------------------------------------------

def simple_barh(
    df,
    x_col,
    y_col,
    figsize=figsize_setting_bar,
    fontsize=fontsize_setting,
    y_min=None,
    y_max=None,
    output_path=None,
    rotation=0,
    color_func=None
):

    # lettertype
    plt.rcParams["font.family"] = font_family_setting

    # kleur
    bar_color = color_func if color_func else get_os_colors(type='discreet', kleur='discreet (1-9)', aantal='1') 

    plt.figure(figsize=figsize)

    bars = plt.bar(
        df[x_col],
        df[y_col],
        color=bar_color
    )

    # y-limieten
    plt.ylim(y_min, y_max)

    # ticks
    plt.xticks(rotation=rotation, fontsize=fontsize)
    plt.yticks(fontsize=fontsize)

    # labels in bars
    for bar, val in zip(bars, df[y_col]):
        plt.text(
            bar.get_x() + bar.get_width() / 2,
            bar.get_height() / 2,
            str(val),
            ha="center",
            va="bottom",
            fontsize=fontsize,
            color="white",
            fontweight="bold"
        )

    ax = plt.gca()

    # tick styling
    ax.tick_params(axis="x", pad=12, length=0)
    ax.tick_params(axis="y", pad=12, length=0)

    # grid
    ax.yaxis.grid(True, linestyle="-", color="#e6e6e6", linewidth=1)
    ax.xaxis.grid(False)
    ax.set_axisbelow(True)

    # spines
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_visible(False)
    ax.spines["bottom"].set_visible(True)

    plt.tight_layout()

    if output_path:
        plt.savefig(output_path, dpi=300, bbox_inches="tight")

    plt.close()
    #plt.show()

#--------------------------------------------------
# GROUPED BAR 
#--------------------------------------------------


def grouped_bar(
    df,
    x_col,
    y_col,
    color_sd,
    color_ams,
    group_col="spatial_name",
    figsize=figsize_setting,
    fontsize=fontsize_setting,
    y_min=None,
    y_max=None,
    output_path=None,
):
    plt.rcParams["font.family"] = font_family_setting

    # Data omzetten naar breed formaat
    pivot_df = df.pivot(
        index=x_col,
        columns=group_col,
        values=y_col
    )

    groups = pivot_df.columns.tolist()

    x = np.arange(len(pivot_df))
    width = 0.35

    fig, ax = plt.subplots(figsize=figsize)
    
    bars1 = ax.bar(
        x - width/2,
        pivot_df[groups[0]],
        width,
        label=groups[0],
        color=color_ams
    )

    bars2 = ax.bar(
        x + width/2,
        pivot_df[groups[1]],
        width,
        label=groups[1],
        color=color_sd
    )

    # labels in de bars
    for bars in [bars1, bars2]:
        for bar in bars:
            height = bar.get_height()
            ax.text(
                bar.get_x() + bar.get_width()/2,
                height/2,
                f"{height:.1f}",
                ha="center",
                va="center",
                color= "white",
                fontsize=fontsize,
                fontweight="bold"
            )

    ax.legend(
        loc="center left",
        bbox_to_anchor=(1, 0.5),
        frameon=False,
        prop={
            "family": font_family_setting,
            "size": fontsize_setting
        },
    )

    ax.set_xticks(x)
    ax.set_xticklabels(
        pivot_df.index,
        rotation=0,
        fontsize=fontsize
    )

    ax.set_ylim(y_min, y_max)

    ax.tick_params(axis="x", labelsize=fontsize, length = 0)
    ax.tick_params(axis="y", labelsize=fontsize, length = 0)

    ax.yaxis.grid(True, linestyle="-", color="#e6e6e6", linewidth=1)
    ax.xaxis.grid(False)
    ax.set_axisbelow(True)

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_visible(False)

#     ax.set_title(
#     f"{df['indicator_sd'].iloc[1]}",
#     fontsize=fontsize_setting,
#     fontfamily=font_family_setting,
#     pad=15
# )
    plt.tight_layout()

    if output_path:
       plt.savefig(output_path, dpi=300, bbox_inches="tight")

    #plt.show()
    plt.close()

#--------------------------------------------------
# LINE PLOT
#--------------------------------------------------
    

def simple_line(
    df,
    x_col,
    y_col,
    figsize=figsize_setting,
    fontsize=fontsize_setting,
    y_min=None,
    y_max=None,
    is_percentage = False,
    output_path=None,
    rotation=0,
    color_func=None,
    marker="o",
    linewidth=3.5
):


    # lettertype
    plt.rcParams["font.family"] = font_family_setting

    # kleur
    line_color = color_func if color_func else get_os_colors(type='discreet', kleur='discreet (1-9)', aantal='1') 

    plt.figure(figsize=figsize)

    plt.plot(
        df[x_col],
        df[y_col],
        color=line_color,
        marker=marker,
        linewidth=linewidth
    )

    plt.legend(
        loc="center left",
        bbox_to_anchor=(1, 0.5),
        frameon=False,
        prop={"family": font_family_setting, "size": fontsize_setting},
    )
    
    # y-limieten
    if y_min is not None or y_max is not None:
        plt.ylim(y_min, y_max)

    # formatter voor %
    ax = plt.gca()

    if is_percentage:
        ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:.0f}%"))

    # ticks
    plt.xticks(rotation=rotation, fontsize=fontsize)
    plt.yticks(fontsize=fontsize)

    ax = plt.gca()

    # tick styling
    ax.tick_params(axis="x", pad=12, length=0)
    ax.tick_params(axis="y", pad=12, length=0)

    # grid
    ax.yaxis.grid(True, linestyle="-", color="#e6e6e6", linewidth=1)
    ax.xaxis.grid(False)
    ax.set_axisbelow(True)

    # spines
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_visible(False)
    ax.spines["bottom"].set_visible(True)

    plt.tight_layout()

    if output_path:
        plt.savefig(output_path, dpi=300, bbox_inches="tight")

    plt.close()
    #plt.show()


#--------------------------------------------------
# Multiple line plot
#--------------------------------------------------


def multiple_line(
    df,
    x_col,
    y_col,
    y_min= None,
    y_max= None,
    main_area = None,
    figsize=figsize_setting,
    fontsize=fontsize_setting,
    is_percentage = False,
    output_path=None,
    rotation=0,
    color_func=None,
    marker="o",
    linewidth=2
):


    # lettertype
    plt.rcParams["font.family"] = font_family_setting

    plt.figure(figsize=figsize)
    
    colors = {
    "Zuidoost": "#004699",
    "Nieuw-West": "#d48fb9",
    "Noord": "#ff9100", #"#6cbd74",
    "Amsterdam":  "#009dec"
    }

    for area, group in df.groupby("spatial_name"):

        plt.plot(
            group[x_col],
            group[y_col],
            label=area,
            color=colors[area],
            alpha = 1 if area == main_area or main_area == "Amsterdam" else 0.4,
            linewidth = 3.5 if area == main_area or main_area == "Amsterdam" else 2.5,
            marker=marker
        )

    plt.legend(
        loc="center left",
        bbox_to_anchor=(1, 0.5),
        frameon=False,
        prop={"family": font_family_setting, "size": fontsize_setting},
    )

    # y-limieten
    if y_min is not None and y_max is not None:
        plt.ylim(y_min, y_max)

    # formatter voor %
    ax = plt.gca()

    if is_percentage:
        ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:.0f}%"))

    # ticks
    plt.xticks(rotation=rotation, fontsize=fontsize)
    plt.yticks(fontsize=fontsize)

    ax = plt.gca()

    # tick styling
    ax.tick_params(axis="x", pad=12, length=0)
    ax.tick_params(axis="y", pad=12, length=0)

    # grid
    ax.yaxis.grid(True, linestyle="-", color="#e6e6e6", linewidth=1)
    ax.xaxis.grid(False)
    ax.set_axisbelow(True)

    # spines
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_visible(False)
    ax.spines["bottom"].set_visible(True)

    plt.tight_layout()

    if output_path:
        plt.savefig(output_path, dpi=300, bbox_inches="tight")

    plt.close()
    #plt.show()