args <- commandArgs(trailingOnly=TRUE)

if(length(args)){
    setwd("D:/AI Mentalist/Imagine Cup/public/collections")
    f_path <- paste0(getwd(),"/",args[1])

    #install package
    list.of.packages <- c("proto", "gsubfn",
    "RSQLite","sqldf",
    "ggplot2","RColorBrewer",
    "wesanderson","scales","magrittr",
    "plotrix","plotly",
    "plyr","jsonlite","rjson","bitops",
    "RCurl","aws.s3","aws.lambda",
    "svglite","googleVis","webshot",
    "dplyr","animation",
    "png","grid","jpeg",
    "htmlwidgets","formattable",
    "htmltools","tibble","gtools",
    "scatterplot3d","plot3D","digest","Rcpp","tidyr")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)

    #library
    library("gtools")
    library('proto')
    library('gsubfn')
    library('RSQLite')
    library('sqldf')
    library('ggplot2')
    library('RColorBrewer')
    library('wesanderson')
    library('scales')
    library('magrittr')
    library('plotrix')
    library('plotly')
    library("plyr")
    library("jsonlite")
    library("rjson")
    library("bitops")
    library("RCurl")
    library("aws.s3")
    library("aws.lambda")
    library('googleVis')
    library('webshot')
    library('dplyr')
    library('animation')
    library('png')
    library('grid')
    library('jpeg')
    library("htmlwidgets")
    library("formattable")
    library("htmltools")
    library("tibble")
    library("gtools")
    library("scatterplot3d")
    library("plot3D")
    library("tidyr")
    library("digest")
    library("Rcpp")

    theme_set(theme_bw() + theme(panel.border = element_blank()))

    create_masterset <- function() {
        path <- paste0(f_path,"/")
        filenames <- list.files(path)
        filenames <- mixedsort(sort(filenames)) #start with 1
        data <- c()
        for (index in 1:length(filenames)) {
            #one csv
            table_data <-
            read.table(paste0(path, filenames[index], sep = ""),
            sep = ",",
            header = TRUE)
            #add csv_number
            table_data[, "csv_number"]  <- c(index)
            #combine all csv
            data <- rbind(data, table_data)
        }

        for (nrow in 1:nrow(data)) {
            data[, "student_id"] <- c(paste0("16000000", data$PeopleCount))
        }
        emotion <<- c("anger",
        "fear",
        "happiness",
        "neutral",
        "sadness",
        "disgust",
        "surprise")
        #col rename
        dataset_2 <-
        data.frame(
        frame_number = data$FrameNumber,
        people_count = data$PeopleCount,
        time = data$FrameTimePosition,
        gender = data$Gender,
        emotion_value_1 = round(data$EmotionsConfidence1, 0),
        emotion_type_1 = data$EmotionsType1,
        emotion_value_2 =  round(data$EmotionsConfidence2, 0),
        emotion_type_2 = data$EmotionsType2,
        emotion_value_3 = round(data$EmotionsConfidence3, 0),
        emotion_type_3 = data$EmotionsType3,
        age_range_high = data$AgeRangeHigh,
        age_range_low = data$AgeRangelow,
        csv_number = data$csv_number,
        student_id = data$student_id
        )

        dataset_final <-
        sqldf(
        c(
        "select dataset_2.*,
        (case when emotion_value_1 >= emotion_value_2 and emotion_value_1 >= emotion_value_3 then emotion_value_1
        when emotion_value_2 >= emotion_value_3 then emotion_value_2
        else emotion_value_3
        end) as max_emotion_value,
        (case when emotion_value_1 >= emotion_value_2 and emotion_value_1 >= emotion_value_3 then emotion_type_1
        when emotion_value_2 >= emotion_value_3 then emotion_type_2
        else emotion_type_3
        end) as max_emotion
        from dataset_2;"
        )
        )
        dataset_final <<- dataset_final[, -c(5:10)]
        min_csv <<- min(dataset_2$csv_number)
        max_csv <<- max(dataset_2$csv_number)
        return(dataset_final)
    }
    masterset <- create_masterset()

    overall_line_chart_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count from data
        group by csv_number,max_emotion"
        )
        return(statement)
    }

    #zero
    bigdataset_emotion_check_zero <- function(data) {
        data <- data %>%
            complete(
            max_emotion = emotion,
            csv_number = c(1:10),
            fill = list(count = 0)
            ) %>%
            select(csv_number, max_emotion, count) %>%
            arrange(csv_number, max_emotion) %>%
            as.data.frame()
        return(data)
    }

    overall_line_chart <- function(data) {
        ylim_max <- max(data$count)
        min <- min(data$csv_number)
        max <- max(data$csv_number)
        chart <- ggplot(data = data,
        aes(x = csv_number,
        y = count,
        group = max_emotion)) +
            geom_line(aes(color = max_emotion)) +
            geom_point(aes(color = max_emotion)) +
            scale_x_continuous(limits = c(min,max),
            breaks = seq(min,max,1)) +
            scale_manual_color() +
            theme_text_size(axis.text.x = element_text(size = 7, face = "bold")) +
            labs(
            title = "THE TREND OF EMOTIONAL CHANGES AMONG STUDENTS",
            x = "LESSON",
            y = "COUNT",
            color = "EMOTION"
            )
        plotly_chart <- ggplotly(chart)

        path <- paste0(f_path,"/overall_line_chart.html")
        htmlwidgets::saveWidget(plotly_chart, file = path,selfcontained = FALSE)
    }

    #chart總體所有學生情緒趨勢

    #anger
    each_emotion_line_chart_anger_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in('anger')
        group by csv_number,max_emotion"
        )
        return(statement)
    }
    #anger :add zero
    anger_check_zero <- function(data) {
        data <- data %>%
            complete(
            max_emotion = "anger",
            csv_number = c(1:10),
            fill = list(count = 0)
            ) %>%
            select(csv_number, max_emotion, count) %>%
            arrange(csv_number, max_emotion) %>%
            as.data.frame()
        return(data)
    }

    #neutral
    each_emotion_line_chart_neutral_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('neutral')
        group by csv_number,max_emotion"
        )
        return(statement)
    }

    #neutral :add zero
    neutral_check_zero <- function(data) {
        data <- data %>%
            complete(
            max_emotion = "neutral",
            csv_number = c(1:10),
            fill = list(count = 0)
            ) %>%
            select(csv_number, max_emotion, count) %>%
            arrange(csv_number, max_emotion) %>%
            as.data.frame()
        return(data)
    }

    #fear
    each_emotion_line_chart_fear_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('fear')
        group by csv_number,max_emotion"
        )
        return(statement)
    }

    #fear :add zero
    fear_check_zero <- function(data) {
        data <- data %>%
            complete(
            max_emotion = "fear",
            csv_number = c(1:10),
            fill = list(count = 0)
            ) %>%
            select(csv_number, max_emotion, count) %>%
            arrange(csv_number, max_emotion) %>%
            as.data.frame()
        return(data)
    }

    #disgust
    each_emotion_line_chart_disgust_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('disgust')
        group by csv_number,max_emotion"
        )
        return(statement)
    }

    #disgust :add zero
    disgust_check_zero <- function(data) {
        data <- data %>%
            complete(
            max_emotion = "disgust",
            csv_number = c(1:10),
            fill = list(count = 0)
            ) %>%
            select(csv_number, max_emotion, count) %>%
            arrange(csv_number, max_emotion) %>%
            as.data.frame()
        return(data)
    }

    #happiness
    each_emotion_line_chart_happiness_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('happiness')
        group by csv_number,max_emotion"
        )
        return(statement)
    }

    #happiness :add zero
    happiness_check_zero <- function(data) {
        data <- data %>%
            complete(
            max_emotion = "happiness",
            csv_number = c(1:10),
            fill = list(count = 0)
            ) %>%
            select(csv_number, max_emotion, count) %>%
            arrange(csv_number, max_emotion) %>%
            as.data.frame()
        return(data)
    }

    #sadness
    each_emotion_line_chart_sadness_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('sadness')
        group by csv_number,max_emotion"
        )
        return(statement)
    }

    #sadness :add zero
    sadness_check_zero <- function(data) {
        data <- data %>%
            complete(
            max_emotion = "sadness",
            csv_number = c(1:10),
            fill = list(count = 0)
            ) %>%
            select(csv_number, max_emotion, count) %>%
            arrange(csv_number, max_emotion) %>%
            as.data.frame()
        return(data)
    }

    #surprise
    each_emotion_line_chart_surprise_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('surprise')
        group by csv_number,max_emotion"
        )
        return(statement)
    }

    #surprise :add zero
    surprise_check_zero <- function(data) {
        data <- data %>%
            complete(
            max_emotion = "surprise",
            csv_number = c(1:10),
            fill = list(count = 0)
            ) %>%
            select(csv_number, max_emotion, count) %>%
            arrange(csv_number, max_emotion) %>%
            as.data.frame()
        return(data)
    }

    #title name
    each_emotion_line_chart <- function(data,emotion_eng,emotion_chin) {
        ylim_max <- as.integer(max(data$count))
        chart <- ggplot(data = data,
        aes(x = csv_number,
        y = count,
        group = max_emotion)) +
            geom_line(aes(color = max_emotion)) +
            geom_point(aes(color = max_emotion)) +
            ylim(0, ylim_max + 25) +
            scale_x_continuous(limits = c(min_csv,
            max_csv),
            breaks = seq(min_csv,
            max_csv,
            1)) +
            scale_manual_color() +
            theme_text_size(axis.text.x = element_text(size = 7, face = "bold")) +
            labs(
            title = paste0("THE TREND OF ",emotion_eng," EMOTIONAL CHANGES AMONG STUDNETS"),
            x = "LESSON",
            y = "COUNT",
            color = "EMOTION"
            )
        plotly_chart <- ggplotly(chart)
        return(plotly_chart)
    }

    each_emotion_line_chart_save <- function (chart, name) {
        all <-  paste0(f_path,
        "/",
        name,
        "_line_chart.html"
        )
        htmlwidgets::saveWidget(chart, file = all,selfcontained = FALSE)

    }

    overall_each_person_sql <- function(data) {
        statement <<-
        sqldf(
        "select max_emotion,people_count,count(max_emotion_value)as count,csv_number from data
        group by people_count,max_emotion,csv_number order by csv_number,people_count"
        )
    }

    overall_personal_emotion_sql <- function(data) {
        count <- function() {
            statement <- list()
            for (people in 1:max(data$people_count)) {
                statement[[people]] <-
                sqldf(
                paste0(
                "select max_emotion,count(max_emotion)as count,csv_number,people_count from data where people_count = ",
                people,
                " group by csv_number,max_emotion"
                )
                )
            }
            return(statement)
        }
        count()

        sum <- function(count_data) {
            sql <- list()
            table <- lapply(count_data, function(data) {
                sql <-
                sqldf(
                "select max_emotion,sum(count) as sum ,people_count from data
                group by max_emotion"
                )

            })
            return(table)
        }
        sum(count_data = count())

    }

    histomgram_bar_total <- function(title, x, y, fill, max) {
        same = list(
        geom_bar(stat = "identity", width = 0.6),
        coord_cartesian(ylim = c(0, max + 25)),
        labs(
        title = title,
        x = x,
        y = y,
        fill = fill
        )
        )
    }

    overall_personal_ggplot_graph <- function(data) {
        chart <- list()
        plotly_chart <- list()

        for (index in 1:length(data)) {
            max_sum_value <- max(data[[index]]$sum)

            chart[[index]] <- ggplot(data = data[[index]],
            aes(x = max_emotion,
            y = sum,
            fill = max_emotion)) +
                histomgram_bar_total(
                title = paste0(
                "THE PERFORMANCE OF EMOTIONS OF STUDENT ID: 16000000",index),
                x = "EMOTION",
                y = "COUNT",
                fill = "EMOTION",
                max = max_sum_value
                ) +
                scale_fill_color() +
                theme_text_size(axis.text.x = element_text(size =7, face = "bold"))

            plotly_chart[[index]] <- ggplotly(chart[[index]])
        }

        size <- length(plotly_chart)
        print_graph <- vector(size, mode = "list")
        all <- paste0(f_path,"/emotion_distribution_person_")
        for (index in 1:length(plotly_chart)) {
            path <- paste0(all, index, ".html")#all
            htmlwidgets::saveWidget(plotly_chart[[index]],selfcontained = FALSE, file = path)
        }
    }

    overall_person_line_chart_sql <- function(data) {
        count <- function() {
            statement <- list()
            for (people in 1:max(data$people_count)) {
                statement[[people]] <-
                sqldf(
                paste0(
                "select max_emotion,count(max_emotion)as count,csv_number,people_count from data where people_count = ",
                people,
                " group by csv_number,max_emotion"
                )
                )
            }
            return(statement)
        }
        emotion <- c("anger",
        "fear",
        "happiness",
        "neutral",
        "sadness",
        "disgust",
        "surprise")

        added_zero_list <- lapply(count(), function(count_data) {
            emotion_list <- count_data %>%
                complete(
                max_emotion = emotion,
                people_count,
                csv_number = c(1:10),
                fill = list(count = 0)
                ) %>%
                select(csv_number, people_count, max_emotion, count) %>%
                arrange(max_emotion, csv_number) %>%
                as.data.frame()
            return(emotion_list)
        })
        added_zero_list

    }

    #overall_person_line_chart_graph
    overall_person_line_chart_graph <- function(data) {
        size <- length(data)
        plotly_chart <- vector(size, mode = "list")
        chart <- vector(size, mode = "list")
        for (index in 1:length(data)) {
            chart[[index]] <- ggplot(data = data[[index]],
            aes(x = csv_number,
            y = count,
            group = max_emotion)) +
                geom_line(aes(color = max_emotion)) +
                geom_point(aes(color = max_emotion)) +
                scale_x_continuous(limits = c(min_csv,
                max_csv),
                breaks = seq(min_csv,
                max_csv,
                1)) +
                scale_manual_color() +
                theme_text_size(axis.text.x = element_text(size = 7, face = "bold")) +
                labs(
                title = paste0("THE TREND OF EMOTIONAL CHANGES OF THE STUDENT ID: 16000000",index),
                x = "LESSON",
                y = "COUNT",
                color = "EMOTION"
                )
            plotly_chart[[index]] <- ggplotly(chart[[index]])
        }

        size <- length(plotly_chart)
        all <- paste0(f_path,"/overall_line_chart_person_")
        for (index in 1:length(plotly_chart)) {
            path <- paste0(all, index,".html") #all
            htmlwidgets::saveWidget(plotly_chart[[index]], file = path,selfcontained = FALSE)
        }
    }

    overall_gender_sentiment_comparision_sql <- function(data) {
        count_emotion_each_person_lesson <- function() {
            statement <-
            sqldf(
            "select max_emotion,count(max_emotion)as count,gender,people_count,csv_number from data
            group by max_emotion,gender,people_count,csv_number order by people_count"
            )
            return(statement)
        }

        sum_emotion_count <- function(data) {
            statement <-
            sqldf(
            "select max_emotion, sum(count) as sum_emotion_count,gender from data
            group by max_emotion,gender"
            )
            return(statement)
        }

        gender_data <- sum_emotion_count(count_emotion_each_person_lesson())
        return(gender_data)
    }

    overall_gender_sentiment_comparision_graph <- function(data) {
        cols <- c("MALE" = "#3276BF","FEMALE"="#EA84C3")

        chart <- ggplot(data, aes(x = max_emotion, y = sum_emotion_count,
        fill = gender)) +
            geom_bar(stat = "identity", width = 0.7) +
            theme(
            axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"), angle = 10)
            ) +
            theme_text_size(axis.text.x = element_text(size = 7, face = "bold")) +
            labs(
            title = "EMOTIONS AMONG DIFFERENT GENDERS",
            x = "\nEMOTION",
            y = "COUNT",
            fill = "GENDER"
            ) +
            coord_flip()+
            scale_fill_manual(values = c("#3276BF","#EA84C3"),limits = c("Male","Female"))
        plotly_chart <- ggplotly(chart)

        path <- paste0(f_path,"/overall_gender_sentiment_comparision.html")
        htmlwidgets::saveWidget(plotly_chart, file =path,selfcontained = FALSE)
    }

    #---------comparison---------

    #anger_compared (student_id)
    compared_person_emotion_line_chart_anger_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('anger')
        group by student_id,csv_number order by csv_number"
        )
        return(statement)
    }

    #neutral_compared (student_id)
    compared_person_emotion_line_chart_neutral_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('neutral')
        group by student_id,csv_number order by csv_number"
        )
        return(statement)
    }

    #fear_compared (student_id)
    compared_person_emotion_line_chart_fear_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('fear')
        group by student_id,csv_number order by csv_number"
        )
        return(statement)
    }

    #disgust_compared (student_id)
    compared_person_emotion_line_chart_disgust_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('disgust')
        group by student_id,csv_number order by csv_number"
        )
        return(statement)
    }

    #happiness_compared (student_id)
    compared_person_emotion_line_chart_happiness_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('happiness')
        group by student_id,csv_number order by csv_number"
        )
        return(statement)
    }

    #sadnessness_compared (student_id)
    compared_person_emotion_line_chart_sadness_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('sadness')
        group by student_id,csv_number order by csv_number"
        )
        return(statement)
    }

    #surprise_compared (student_id)
    compared_person_emotion_line_chart_surprise_sql <- function(data) {
        statement <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('surprise')
        group by student_id,csv_number order by csv_number"
        )
        return(statement)
    }

    compared_overall_emotion_line_chart <- function(data,emotion_eng,emotion_chin) {
        data <- data %>%
            complete(max_emotion,
            csv_number = c(1:10),
            fill = list(count = 0),
            student_id) %>%
            select(csv_number, student_id, count, max_emotion) %>%
            arrange(csv_number, student_id) %>% #order the data frame by this col
            as.data.frame()

        data$student_id <- as.factor(data$student_id)
        min <- min(data$csv_number)
        max <- max(data$csv_number)
        chart <- ggplot(data = data,
        aes(x = csv_number,
        y = count,
        group = student_id)) +
            geom_line(aes(color = student_id)) +
            geom_point(aes(color = student_id)) +
            scale_x_continuous(limits = c(min,
            max),
            breaks = seq(min,
            max,
            1)) +

            theme_text_size(axis.text.x = element_text(size = 7, face = "bold")) +
            labs(
            title = paste0("THE TREND OF ",emotion_eng," AMONG STUDENTS"),
            x = "LESSON",
            y = "COUNT",
            color = "STUDENT"
            )

        plotly_chart <- ggplotly(chart)
        return (plotly_chart)
    }

    compared_overall_emotion_line_chart_save <- function(chart,name){
        all <-  paste0(f_path,"/compared_overall_",name,"_emotion.html")
        htmlwidgets::saveWidget(chart, file = all,selfcontained = FALSE)
    }


    #------------pie chart---------------
    overall_anger_emotion_pie_chart_sql <- function(data) {

        count_emotion <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('anger')
        group by student_id,csv_number order by csv_number"
        )
        max_anger_person <-
        sqldf(
        "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
        group by csv_number order by csv_number"
        )
        count_the_frequency <-
        sqldf(
        "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_anger_person
        group by student_id"
        )
        count_the_frequency$percentage <-round(count_the_frequency$frequency/sum(count_the_frequency$frequency)*100,0)
        return(count_the_frequency)
    }

    overall_anger_emotion_pie_chart <- function(data){
        chart_margin <- list(
        l = 150,
        #Sets the left margin
        r = 150,
        #Sets the right margin
        b = 10,
        #Sets the bottom margin
        t = 120,
        #Sets the top margin
        pad = 20,
        #Sets the amount of padding (in px) between the plotting area and the axis lines
        autoexpand = TRUE
        )

        chart <-
        plot_ly(
        data,
        labels = ~ student_id,
        values = ~ percentage,
        type = 'pie'
        ) %>%    #legend

            add_annotations( text="STUDENT", xref="paper", yref="paper",
            x=0.9, xanchor="left",
            y=0.8, yanchor="bottom",    # Same y as legend below
            legendtitle=TRUE, showarrow=FALSE ) %>%
            layout(
            title = 'THE PERCENTAGE OF ANGER AMONG STUDENTS\n',
            showlegend = T,
            xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            legend=list(y=0.8, yanchor="top",
            x=0.9, xanchor="left"),
            font = list(size = 9),margin = chart_margin
            )

        path <-  paste0(f_path,"/overall_anger_emotion_pie_chart.html")
        htmlwidgets::saveWidget(chart, file = path,selfcontained = FALSE)
    }

    overall_neutral_emotion_pie_chart_sql <- function(data) {

        count_emotion <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('neutral')
        group by student_id,csv_number order by csv_number"
        )
        max_neutral_person <-
        sqldf(
        "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
        group by csv_number order by csv_number"
        )
        count_the_frequency <-
        sqldf(
        "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_neutral_person
        group by student_id"
        )
        count_the_frequency$percentage <-round(count_the_frequency$frequency/sum(count_the_frequency$frequency)*100,0)
        return(count_the_frequency)
    }

    overall_neutral_emotion_pie_chart <- function(data){
        chart_margin <- list(
        l = 150,
        #Sets the left margin
        r = 150,
        #Sets the right margin
        b = 10,
        #Sets the bottom margin
        t = 120,
        #Sets the top margin
        pad = 20,
        #Sets the amount of padding (in px) between the plotting area and the axis lines
        autoexpand = TRUE
        )

        chart <-
        plot_ly(
        data,
        labels = ~ student_id,
        values = ~ percentage,
        type = 'pie'
        ) %>%    #legend

            add_annotations( text="STUDENT", xref="paper", yref="paper",
            x=0.9, xanchor="left",
            y=0.8, yanchor="bottom",    # Same y as legend below
            legendtitle=TRUE, showarrow=FALSE ) %>%
            layout(
            title = 'THE PERCENTAGE OF neutral AMONG STUDENTS\n',
            showlegend = T,
            xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            legend=list(y=0.8, yanchor="top",
            x=0.9, xanchor="left"),
            font = list(size = 9),margin = chart_margin
            )

        path <-  paste0(f_path,"/overall_neutral_emotion_pie_chart.html")
        htmlwidgets::saveWidget(chart, file = path,selfcontained = FALSE)
    }

    overall_fear_emotion_pie_chart_sql <- function(data) {

        count_emotion <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('fear')
        group by student_id,csv_number order by csv_number"
        )
        max_fear_person <-
        sqldf(
        "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
        group by csv_number order by csv_number"
        )
        count_the_frequency <-
        sqldf(
        "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_fear_person
        group by student_id"
        )
        count_the_frequency$percentage <-round(count_the_frequency$frequency/sum(count_the_frequency$frequency)*100,0)
        return(count_the_frequency)
    }

    overall_fear_emotion_pie_chart <- function(data){
        chart_margin <- list(
        l = 150,
        #Sets the left margin
        r = 150,
        #Sets the right margin
        b = 10,
        #Sets the bottom margin
        t = 120,
        #Sets the top margin
        pad = 20,
        #Sets the amount of padding (in px) between the plotting area and the axis lines
        autoexpand = TRUE
        )

        chart <-
        plot_ly(
        data,
        labels = ~ student_id,
        values = ~ percentage,
        type = 'pie'
        ) %>%    #legend

            add_annotations( text="STUDENT", xref="paper", yref="paper",
            x=0.9, xanchor="left",
            y=0.8, yanchor="bottom",    # Same y as legend below
            legendtitle=TRUE, showarrow=FALSE ) %>%
            layout(
            title = 'THE PERCENTAGE OF CONFUSION AMONG STUDENTS\n',
            showlegend = T,
            xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            legend=list(y=0.8, yanchor="top",
            x=0.9, xanchor="left"),
            font = list(size = 9),margin = chart_margin
            )

        path <-  paste0(f_path,"/overall_fear_emotion_pie_chart.html")
        htmlwidgets::saveWidget(chart, file = path,selfcontained = FALSE)
    }

    overall_disgust_emotion_pie_chart_sql <- function(data) {

        count_emotion <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('disgust')
        group by student_id,csv_number order by csv_number"
        )
        max_disgust_person <-
        sqldf(
        "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
        group by csv_number order by csv_number"
        )
        count_the_frequency <-
        sqldf(
        "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_disgust_person
        group by student_id"
        )
        count_the_frequency$percentage <-round(count_the_frequency$frequency/sum(count_the_frequency$frequency)*100,0)
        return(count_the_frequency)
    }

    overall_disgust_emotion_pie_chart <- function(data){
        chart_margin <- list(
        l = 150,
        #Sets the left margin
        r = 150,
        #Sets the right margin
        b = 10,
        #Sets the bottom margin
        t = 120,
        #Sets the top margin
        pad = 20,
        #Sets the amount of padding (in px) between the plotting area and the axis lines
        autoexpand = TRUE
        )

        chart <-
        plot_ly(
        data,
        labels = ~ student_id,
        values = ~ percentage,
        type = 'pie'
        ) %>%    #legend

            add_annotations( text="STUDENT", xref="paper", yref="paper",
            x=0.9, xanchor="left",
            y=0.8, yanchor="bottom",    # Same y as legend below
            legendtitle=TRUE, showarrow=FALSE ) %>%
            layout(
            title = 'THE PERCENTAGE OF DISGUST AMONG STUDENTS\n',
            showlegend = T,
            xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            legend=list(y=0.8, yanchor="top",
            x=0.9, xanchor="left"),
            font = list(size = 9),margin = chart_margin
            )

        path <-  paste0(f_path,"/overall_disgust_emotion_pie_chart.html")
        htmlwidgets::saveWidget(chart, file = path,selfcontained = FALSE)
    }

    overall_happiness_emotion_pie_chart_sql <- function(data) {

        count_emotion <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('happiness')
        group by student_id,csv_number order by csv_number"
        )
        max_happiness_person <-
        sqldf(
        "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
        group by csv_number order by csv_number"
        )
        count_the_frequency <-
        sqldf(
        "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_happiness_person
        group by student_id"
        )
        count_the_frequency$percentage <-round(count_the_frequency$frequency/sum(count_the_frequency$frequency)*100,0)
        return(count_the_frequency)
    }

    overall_happiness_emotion_pie_chart <- function(data){
        chart_margin <- list(
        l = 150,
        #Sets the left margin
        r = 150,
        #Sets the right margin
        b = 10,
        #Sets the bottom margin
        t = 120,
        #Sets the top margin
        pad = 20,
        #Sets the amount of padding (in px) between the plotting area and the axis lines
        autoexpand = TRUE
        )

        chart <-
        plot_ly(
        data,
        labels = ~ student_id,
        values = ~ percentage,
        type = 'pie'
        ) %>%    #legend

            add_annotations( text="STUDENT", xref="paper", yref="paper",
            x=0.9, xanchor="left",
            y=0.8, yanchor="bottom",    # Same y as legend below
            legendtitle=TRUE, showarrow=FALSE ) %>%
            layout(
            title = 'THE PERCENTAGE OF HAPPINESS AMONG STUDENTS\n',
            showlegend = T,
            xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            legend=list(y=0.8, yanchor="top",
            x=0.9, xanchor="left"),
            font = list(size = 9),margin = chart_margin
            )

        path <-  paste0(f_path,"/overall_happiness_emotion_pie_chart.html")
        htmlwidgets::saveWidget(chart, file = path,selfcontained = FALSE)
    }

    overall_sadness_emotion_pie_chart_sql <- function(data) {

        count_emotion <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('sadness')
        group by student_id,csv_number order by csv_number"
        )
        max_sadness_person <-
        sqldf(
        "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
        group by csv_number order by csv_number"
        )
        count_the_frequency <-
        sqldf(
        "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_sadness_person
        group by student_id"
        )
        count_the_frequency$percentage <-round(count_the_frequency$frequency/sum(count_the_frequency$frequency)*100,0)
        return(count_the_frequency)
    }

    overall_sadness_emotion_pie_chart <- function(data){
        chart_margin <- list(
        l = 150,
        #Sets the left margin
        r = 150,
        #Sets the right margin
        b = 10,
        #Sets the bottom margin
        t = 120,
        #Sets the top margin
        pad = 20,
        #Sets the amount of padding (in px) between the plotting area and the axis lines
        autoexpand = TRUE
        )

        chart <-
        plot_ly(
        data,
        labels = ~ student_id,
        values = ~ percentage,
        type = 'pie'
        ) %>%    #legend

            add_annotations( text="STUDENT", xref="paper", yref="paper",
            x=0.9, xanchor="left",
            y=0.8, yanchor="bottom",    # Same y as legend below
            legendtitle=TRUE, showarrow=FALSE ) %>%
            layout(
            title = 'THE PERCENTAGE OF sadnessNESS AMONG STUDENTS\n',
            showlegend = T,
            xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            legend=list(y=0.8, yanchor="top",
            x=0.9, xanchor="left"),
            font = list(size = 9),margin = chart_margin
            )

        path <-  paste0(f_path,"/overall_sadness_emotion_pie_chart.html")
        htmlwidgets::saveWidget(chart, file = path,selfcontained = FALSE)
    }

    overall_surprise_emotion_pie_chart_sql <- function(data) {

        count_emotion <-
        sqldf(
        "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('surprise')
        group by student_id,csv_number order by csv_number"
        )
        max_surprise_person <-
        sqldf(
        "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
        group by csv_number order by csv_number"
        )
        count_the_frequency <-
        sqldf(
        "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_surprise_person
        group by student_id"
        )
        count_the_frequency$percentage <-round(count_the_frequency$frequency/sum(count_the_frequency$frequency)*100,0)
        return(count_the_frequency)
    }

    overall_surprise_emotion_pie_chart <- function(data){
        chart_margin <- list(
        l = 150,
        #Sets the left margin
        r = 150,
        #Sets the right margin
        b = 10,
        #Sets the bottom margin
        t = 120,
        #Sets the top margin
        pad = 20,
        #Sets the amount of padding (in px) between the plotting area and the axis lines
        autoexpand = TRUE
        )

        chart <-
        plot_ly(
        data,
        labels = ~ student_id,
        values = ~ percentage,
        type = 'pie'
        ) %>%    #legend

            add_annotations( text="STUDENT", xref="paper", yref="paper",
            x=0.9, xanchor="left",
            y=0.8, yanchor="bottom",    # Same y as legend below
            legendtitle=TRUE, showarrow=FALSE ) %>%
            layout(
            title = 'THE PERCENTAGE OF SURPRISE AMONG STUDENTS\n',
            showlegend = T,
            xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
            ),
            legend=list(y=0.8, yanchor="top",
            x=0.9, xanchor="left"),
            font = list(size = 9),margin = chart_margin
            )

        path <-  paste0(f_path,"/overall_surprise_emotion_pie_chart.html")
        htmlwidgets::saveWidget(chart, file = path,selfcontained = FALSE)
    }

    #Other function
    histomgram_bar <- function(title, x, y, fill) {
        same = list(
        geom_bar(stat = "identity", width = 0.6),
        coord_cartesian(ylim = c(0, 100)),
        labs(
        title = title,
        x = x,
        y = y,
        fill = fill
        )
        )
    }

    scale_fill_color <- function() {
        color = list(scale_fill_manual(
        values = c(
        "#F8766D",
        "#C49A00",
        "#53B400",
        "#00C094",
        "#00B6EB",
        "#A58AFF",
        "#FB61D7"
        ),
        limits = c(
        "anger",
        "fear",
        "happiness",
        "neutral",
        "sadness",
        "disgust",
        "surprise"
        )
        ))
    }

    scale_manual_color <- function(){
        color = list(scale_color_manual(
        #breaks=c("anger","fear","happiness","neutral","sadness","disgust","surprise"),
        values = c(
        "#F8766D",
        "#C49A00",
        "#53B400",
        "#00C094",
        "#00B6EB",
        "#A58AFF",
        "#FB61D7"
        ),
        limits = c(
        "anger",
        "fear",
        "happiness",
        "neutral",
        "sadness",
        "disgust",
        "surprise"
        )
        ))
    }
    theme_text_size <- function(axis.text.x) {
        theme(
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text = element_text(size = 8),
        axis.text.x = axis.text.x,
        # axis.text.x = element_text(size = rel(1.5),face="bold")
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),#margin = margin(t = 0, r = 0, b = 0, l = 20)
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.margin =  unit(c(1,1,1,1), "cm") #3:bottom
        )
    }

    #------------------------------histogram---------------------
    #histogram chart一名學生所有堂數的情緒表現: 16000000"
    overall_personal_ggplot_graph(overall_personal_emotion_sql(masterset))
    #overall_gender_sentiment_comparision_graph總體男女心情對比圖
    overall_gender_sentiment_comparision_graph(overall_gender_sentiment_comparision_sql(masterset))

    #------------------------------line_chart---------------------
    #總體所有學生情緒趨勢
    overall_line_chart(bigdataset_emotion_check_zero(overall_line_chart_sql(masterset)))
    #anger總體所有學生每種的情緒趨勢
    each_emotion_line_chart_save(each_emotion_line_chart(anger_check_zero(each_emotion_line_chart_anger_sql(masterset)),emotion_eng="ANGER",emotion_chin="生氣")
    ,name="anger")
    #neutral總體所有學生每種的情緒趨勢
    each_emotion_line_chart_save(each_emotion_line_chart(neutral_check_zero(each_emotion_line_chart_neutral_sql(masterset)),emotion_eng="neutral",emotion_chin="冷靜")
    ,name="neutral")
    #fear總體所有學生每種的情緒趨勢
    each_emotion_line_chart_save(each_emotion_line_chart(fear_check_zero(each_emotion_line_chart_fear_sql(masterset)),emotion_eng="CONFUSION",emotion_chin="迷惘")
    ,name="fear")
    #disgust總體所有學生每種的情緒趨勢
    each_emotion_line_chart_save(each_emotion_line_chart(disgust_check_zero(each_emotion_line_chart_disgust_sql(masterset)),emotion_eng="DISGUST",emotion_chin="厭惡")
    ,name="disgust")
    #happiness總體所有學生每種的情緒趨勢
    each_emotion_line_chart_save(each_emotion_line_chart(happiness_check_zero(each_emotion_line_chart_happiness_sql(masterset)),emotion_eng="HAPPINESS",emotion_chin="開心")
    ,name="happiness")
    #sadness總體所有學生每種的情緒趨勢
    each_emotion_line_chart_save(each_emotion_line_chart(sadness_check_zero(each_emotion_line_chart_sadness_sql(masterset)),emotion_eng="sadnessNESS",emotion_chin="傷心")
    ,name="sadness")
    #surprise總體所有學生每種的情緒趨勢
    each_emotion_line_chart_save(each_emotion_line_chart(surprise_check_zero(each_emotion_line_chart_surprise_sql(masterset)),emotion_eng="SURPRISE",emotion_chin="驚訝")
    ,name="surprise")
    #overall_person_line_chart_graph每個學生編號每種情緒的趨勢
    overall_person_line_chart_graph(overall_person_line_chart_sql(masterset))

    #--------comparison----#
    #anger
    compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(
    compared_person_emotion_line_chart_anger_sql(masterset),emotion_eng="ANGER",emotion_chin="生氣"),name="anger")
    #neutral
    compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(
    compared_person_emotion_line_chart_neutral_sql(masterset),emotion_eng="neutral",emotion_chin="冷靜"),name="neutral")
    #confusion
    compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(
    compared_person_emotion_line_chart_fear_sql(masterset),emotion_eng="CONFUSION",emotion_chin="迷惘"),name="fear")
    #disgust
    compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(
    compared_person_emotion_line_chart_disgust_sql(masterset),emotion_eng="DISGUST",emotion_chin="厭惡"),name="disgust")
    #happiness
    compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(
    compared_person_emotion_line_chart_happiness_sql(masterset),emotion_eng="HAPPINESS",emotion_chin="開心"),name="happiness")
    #sadnessness
    compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(
    compared_person_emotion_line_chart_sadness_sql(masterset),emotion_eng="sadnessNESS",emotion_chin="傷心"),name="sadness")
    #surprise
    compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(
    compared_person_emotion_line_chart_surprise_sql(masterset),emotion_eng="SURPRISE",emotion_chin="驚訝"),name="surprise")

    #--------pie chart----#
    #the angriest
    overall_anger_emotion_pie_chart(overall_anger_emotion_pie_chart_sql(masterset))
    #the neutralest
    overall_neutral_emotion_pie_chart(overall_neutral_emotion_pie_chart_sql(masterset))
    #the fear students
    overall_fear_emotion_pie_chart(overall_fear_emotion_pie_chart_sql(masterset))
    #the disgust students
    overall_disgust_emotion_pie_chart(overall_disgust_emotion_pie_chart_sql(masterset))
    #the happiness students
    overall_happiness_emotion_pie_chart(overall_happiness_emotion_pie_chart_sql(masterset))
    #the sadness students
    overall_sadness_emotion_pie_chart(overall_sadness_emotion_pie_chart_sql(masterset))
    #the surprise students
    overall_surprise_emotion_pie_chart(overall_surprise_emotion_pie_chart_sql(masterset))
}