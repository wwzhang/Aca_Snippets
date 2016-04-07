##Games-Howell test##

https://statistics.laerd.com/statistical-guides/one-way-anova-statistical-guide-3.php


		tukey <- function(   data,                                   # 観察値ベクトル
		                        group,                                  # 群変数ベクトル
		                        method=c("Tukey", "Games-Howell"))      # 手法の選択
		{
		        OK <- complete.cases(data, group)                    # 欠損値を持つケースを除く
		        data <- data[OK]
		        group <- factor(group[OK])
		        n <- tapply(data, group, length)                     # 各群のケース数
		        a <- length(n)                                               # 群の数
		        phi.e <- sum(n)-a                                    # 誤差分散（群内不偏分散）の自由度
		        Mean <- tapply(data, group, mean)                    # 各群の平均値
		        Variance <- tapply(data, group, var)                 # 各群の不偏分散
		        result1 <- cbind(n, Mean, Variance)                  # 各群の統計量
		        rownames(result1) <- paste("Group", 1:a, sep="")
		        method <- match.arg(method)
		        if (method == "Tukey") {                                # Tukey の方法
		                v.e <- sum((n-1)*Variance)/phi.e             # 誤差分散（群内不偏分散）
		                t <- combn(a, 2, function(ij)                        # 対比較
		                                        abs(diff(Mean[ij]))/sqrt(v.e*sum(1/n[ij])) )
		                p <- ptukey(t*sqrt(2), a, phi.e, lower.tail=FALSE)   # 有意確率を計算する
		                Tukey <- cbind(t, p)                                 # 対比較の結果
		                rownames(Tukey) <- combn(a, 2, paste, collapse=":")
		                return(list(result1=result1, Tukey=Tukey, phi=phi.e, v=v.e))
		        }
		        else {                                                  # Games-Howell の方法
		                t.df <- combn(a, 2, function(ij) {           # 対比較
		                                        t <- abs(diff(Mean[ij]))/sqrt(sum(Variance[ij]/n[ij]))
		                                        df <- sum(Variance[ij]/n[ij])^2/sum((Variance[ij]/n[ij])^2/(n[ij]-1))
		                                        return(c(t, df))} )
		                t <- t.df[1,]
		                df <- t.df[2,]
		                p <- ptukey(t*sqrt(2), a, df, lower.tail=FALSE)      # 有意確率を計算する
		                Games.Howell <- cbind(t, df, p)                      # 対比較の結果
		                rownames(Games.Howell) <- combn(a, 2, paste, collapse=":")
		                return(list(result1=result1, Games.Howell=Games.Howell))
		        }
		}
		tukey(tablets$values, tablets$ind, method = "Games-Howell")
