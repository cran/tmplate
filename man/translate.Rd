\name{translate}
\alias{translate}
\title{
Translate tags and R code in template
}
\description{
Evaluate tags and inline or chunks of R code present in general template files to produce variable content depending on some input arguments.
}
\usage{
translate(vars, ..., template, drop = FALSE, default = "NULL",
        warn = TRUE, start = "<:", end = ":>", redo = 2,
        envir = new.env(parent = parent.frame()), retrans = 2,
        debug = FALSE)
}
\arguments{
	\item{vars}{
		list with named elements containing arbitrary variables to search in templates. To specify variables should be used one of the arguments \code{vars} or \dots but not both. If both are defined only \code{vars} is used.
	}
	\item{...}{
		arbitrary variables to search in templates.
	}
	\item{template}{
		path to template to evaluate.
	}
	\item{drop}{
		drop lines with non evaluated variables? Default to FALSE.
	}
	\item{default}{
		default character to translate for not given or missing variables. Default to "NULL" (will appear preceded by "<:" and ":>" or \code{startNULLend} in translated file).
	}
	\item{warn}{
		print warning if some variables are still not translated in the finel file. Default to TRUE. If FALSE process will stop returning an error.
	}
	\item{start}{
		line number where the R code chunk starts, or string to match start of variable. Default to "<:".
	}
	\item{end}{
		line number where the R code chunk ends, or string to match end of variable. Default to ":>".
	}
	\item{redo}{
		how many times translate? Default to 2 (so variables can appear in definition of other variables and still be translated).
	}
	\item{retrans}{
		retranslate a template a number of times.
	}
	\item{envir}{
		environment where to evaluate R code.
	}
	\item{debug}{
		logical. Default to FALSE (no debug).
	}

}
\details{
The function \code{translate} evaluate 'tags' and inline or chunks of R code in a template
modifying its content. The tags are calls that point to argument variables which are provided by the user. 
Thus, the main input arguments for \code{translate} are the template (provided as a character vector 
where each element correspond to a line in the template file) and the variables which will be used 
to evaluate the tags in the template to update its content. The evaluation is performed in an environment 
which can also be defined by the user. The output of \code{translate} is a character vector 
where each element correspond to the line of the modified template. 
The tags names are denoted in betweed \code{<:} \code{:>}, for example \code{<:tag_name:>}, 
and they must be placed in the position where we want their evaluation to occur and output to appear. 
We should provide \code{tag_name = value} in the arguments of function \code{translate}. 
The inline or chunks of R code must follow the rules set in \link[tRnslate]{translate_r_code}. 
See \href{../../tRnslate/doc/tRnslate.html}{tRnslate package vignette} or run \code{vignette(tRnslate)}.
For more details see \href{../doc/tmplate.html}{tmplate package vignette} or run \code{vignette("tmplate")}.
For an example see \link{tmplate-package}.
}
\value{
Once tags and the chunks or inline R code are evaluated by \code{translate} using the input arguments given, then it returns a character vector where each element corresponds to the original line in the template file where the tags, chunks and inline code has been replaced by its output. This content can be seen in console or written to disc, for example, by using \code{cat} (it requires to use \code{sep = "\\n"}).
}
\author{
Mario A. Martinez Araya, \email{r@marioma.me}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~ 
\examples{
## To see an example in R console run:
##
## ?tmplate::tmplate
##
## Or See instructions in the following file:
## (example present in the vignette)
source(system.file("examples/example.R", package = "tmplate"))
}

