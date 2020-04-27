
library(motifr)

# closed and open triangles, one node on level 0, two nodes on level 1

motifs = list('1,2[I.C]', '1,2[II.C]')
motif = c('1,2[II.C]') # closed triangle

count_motifs(ml_net, motifs = motifs, lvl_attr = "sesType")
motifs_distribution(ml_net, motifs = motifs)
motif_summary(ml_net)
exemplify_motif(ml_net, motif = motif)
show_motif(ml_net, motif = motif)
show_motif(ml_net, motif = motif, label = TRUE)
simulate_baseline(ml_net, motifs = motifs, n = 20)

plot_mnet(net = dummy_net,lvl_attr = "sesType")

compare_to_baseline(ml_net, motifs = motifs, n = 10) #works
compare_to_baseline(ml_net, motifs = motifs, n = 100) #bug

identify_gaps(ml_net, motif = motif)
critical_dyads(ml_net, motif = motif)

plot_gaps(ml_net, motif = motif, label = TRUE, cutoff = 3)
